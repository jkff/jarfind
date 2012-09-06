{-# LANGUAGE BangPatterns #-}

module JarFind
( Class(..)
, Member(..)
, Access(..)
, ClassFileSource(..)
, Location(..)
, parseClassFile
, parseFileSource
) where

import Codec.Archive.Zip
import qualified Data.ByteString.Lazy as B
import Control.Monad
import Control.Applicative ((<$>))
import Data.List
import Data.Bits
import Data.Maybe
import Data.Char

import Data.Array.ST
import Data.Array.Unboxed

import Data.Binary
import Data.Binary.Get

import System.Console.GetOpt
import System.Environment

import Text.Regex.TDFA

data Class = Class { clsName        :: String
                   , clsAccess      :: Access
                   , clsIsInterface :: Bool
                   , clsMembers     :: [Member]
                   }
             deriving Show

data Member = Method { mName   :: String
                     , mAccess :: Access
                     , mSig    :: String
                     }
            | Field  { mName   :: String
                     , mAccess :: Access
                     , mSig    :: String
                     }
              deriving Show

data Access = Public
            | Private
            | Protected
            | Package
              deriving Eq

instance Show Access where
    show Public    = "public"
    show Private   = "private"
    show Protected = "protected"
    show Package   = ""

----------------------------------------------------------------
--                      CLASS FILE PARSING                    --
----------------------------------------------------------------

-- Constant pool stuff

data ConstantPool = ConstantPool
    { cpPos  :: UArray Word16 Word16
    , cpData :: B.ByteString
    }

getUTF8FromCP :: ConstantPool -> Word16 -> B.ByteString
getUTF8FromCP cp@(ConstantPool cpPos cpData) i = res
    where pos = cpPos!i
          begin = fromIntegral $ pos+3
          len   = fromIntegral $ getWord16FromCP cp (pos+1)
          res = B.take len (B.drop begin cpData)

getWord16FromCP :: Integral a => ConstantPool -> a -> Word16
getWord16FromCP (ConstantPool cpPos cpData) iData = 
    (do skip (fromIntegral iData); readWord16) `runGet` cpData

-- The very class file parser

readByte :: Get Word8
readByte = get

readWord16 :: Get Word16
readWord16 = get

readInt16 :: Get Int
readInt16 = fromIntegral `liftM` readWord16

readWord32 :: Get Word32
readWord32 = get

parseClassFile :: B.ByteString -> Class
parseClassFile = runGet $ do
    skip (4 + 2 + 2) -- magic, minor_version, major_version
    cpSize      <- readWord16
    cp          <- parseConstantPool cpSize
    accessFlags <- readWord16
    thisClass   <- readWord16
    let classNameIndex = getWord16FromCP cp (1 + (cpPos cp)!thisClass) 
    let className      = getUTF8FromCP   cp classNameIndex
    let access         = flagsToAccess accessFlags
    let isInterface    = (0 /= accessFlags .&. 0x0200)
    skip 2                    -- super_class
    ifCount     <- readInt16  -- interfaces_count
    skip (2 * ifCount)          -- u2 interfaces[interfaces_count]
    fieldCount  <- readInt16
    fields      <- replicateM fieldCount (parseMember cp Field decryptType) 
    methodCount <- readInt16
    methods     <- replicateM methodCount (parseMember cp Method decryptSig)
    return (Class (bToString className) access isInterface (fields++methods))

parseMember :: ConstantPool -> 
               (String -> Access -> String -> Member) ->
               (B.ByteString -> B.ByteString) ->
               Get Member
parseMember cp ctor decryptor = do
    accessFlags <- readWord16
    nameIndex   <- readWord16
    descrIndex  <- readWord16
    attCount    <- readInt16
    replicateM attCount $ do
                            skip 2
                            attLen <- readWord32
                            skip (fromIntegral attLen)

    return $ ctor (bToString (getUTF8FromCP cp nameIndex))
                  (flagsToAccess accessFlags)
                  (bToString (decryptor (getUTF8FromCP cp descrIndex)))

-- A strict unboxed list and conversion to an UArray with STUArray's
data List' a = Nil' 
             | (:!) !a  
                    !(List' a)
infixr 5 :!
toArray' (lo,hi) list = do
    arr <- newArray_ (lo,hi)
    fill hi list arr
    return arr
fill i list arr = fill' i list
    where fill' !i (b:!bs') = writeArray arr i b >> fill' (i-1) bs'
          fill' !i Nil'     = return ()

parseConstantPool :: Word16 -> Get ConstantPool    
parseConstantPool n = do
    bs         <- getRemainingLazyByteString
    cpPosList  <- parseConstantPool' (n-1) 0 (0 :! Nil')
    cpPosArray <- return $ runSTUArray (toArray' (0,n-1) cpPosList)
    return (ConstantPool cpPosArray bs)

parseConstantPool' :: Word16 -> Word16 -> List' Word16 -> Get (List' Word16)
parseConstantPool' 0 pos res = return res
parseConstantPool' n pos res = do
  tag <- readByte
  let skipRet n = do skip n; return n
  delta <- fromIntegral `liftM` case tag of
    1  -> do len <- readInt16  -- CONSTANT_Utf8
             skip len
             return (len+2)   
    2  -> error "Tag 2 is unused"
    3  -> skipRet 4            -- CONSTANT_Integer
    4  -> skipRet 4            -- CONSTANT_Float
    5  -> skipRet 8            -- CONSTANT_Long (2 slots)
    6  -> skipRet 8            -- CONSTANT_Double (2 slots)
    7  -> skipRet 2            -- CONSTANT_Class
    8  -> skipRet 2            -- CONSTANT_String
    9  -> skipRet 4            -- CONSTANT_Fieldref
    10 -> skipRet 4            -- CONSTANT_Methodref
    11 -> skipRet 4            -- CONSTANT_InterfaceMethodref
    12 -> skipRet 4            -- CONSTANT_NameAndType
    _  -> error $ "Unknown tag "++show tag++" at position "++show pos++" in CP"
  let n' = if (tag==5 || tag==6) then (n-2) else (n-1)
  let p = pos+delta+1 
  p `seq` case tag of
    5 -> parseConstantPool' n' p (pos :! (0 :! res))
    6 -> parseConstantPool' n' p (pos :! (0 :! res))
    _ -> parseConstantPool' n' p (pos :! res)


-- Field/method type decryption

decryptType :: B.ByteString -> B.ByteString
decryptType = onString (fst . decryptType')

decryptType' :: String -> (String, String)
decryptType' ""      = ("","")
decryptType' (a:s) = case a of
    'B' -> ("byte",s)
    'C' -> ("char",s)
    'D' -> ("double",s)
    'F' -> ("float",s)
    'I' -> ("int",s)
    'J' -> ("long",s)
    'S' -> ("short",s)
    'Z' -> ("boolean",s)
    'V' -> ("void",s)
    '[' -> let (t,s') = decryptType' s in (t++"[]",s')
    'L' -> go "" s
        where go c (';':s) = (reverse c, s)
              go c ('/':s) = go ('.':c) s
              go c (a:s)   = go (a:c)   s
              go _ _       = error $ "Unrecognized type ending with "++s
    _   -> error $ "Unrecognized type: "++s

decryptSig :: B.ByteString -> B.ByteString
decryptSig = onString decryptSig'

decryptSig' :: String -> String
decryptSig' s = concat (if null parTypes 
                        then ["()"]
                        else (intersperse " -> " parTypes)) 
                ++ " -> " ++ retType
    where ('(':params, ')':ret) = break (==')') s
          retType = fst (decryptType' ret)
          parTypes = go [] params
          go ps "" = reverse ps
          go ps s  = let (p, s') = decryptType' s in go (p:ps) s'


flagsToAccess :: Word16 -> Access
flagsToAccess w | 0 /= w.&.0x0001 = Public
                | 0 /= w.&.0x0002 = Private
                | 0 /= w.&.0x0004 = Protected
                | otherwise       = Package


bToString :: B.ByteString -> String
bToString = map (chr . fromIntegral) . B.unpack

bFromString :: String -> B.ByteString
bFromString = B.pack . map (fromIntegral . ord)

onString :: (String -> String) -> (B.ByteString -> B.ByteString)
onString f = bFromString . f . bToString

data ClassFileSource = ClassFile { path  :: FilePath }
                     | JarFile   { path  :: FilePath }
                     | ClassPath { paths :: [ClassFileSource] }

data Location = InFile FilePath
              | InJar { jarPath   :: FilePath
                      , innerPath :: FilePath
                      }
                deriving Show

parseFileSource :: ClassFileSource -> IO [(Location, Class)]
parseFileSource (ClassFile path) = do
    f <- parseClassFile <$> B.readFile path
    return [(InFile path, f)]
parseFileSource (JarFile jar)    = do
    archive <- toArchive <$> B.readFile jar
    return [(InJar jar fileName, parseClassFile $ fromEntry entry)
            | entry <- zEntries archive,
              let fileName = eRelativePath entry,
              ".class" `isSuffixOf` fileName]
parseFileSource (ClassPath paths) = concat <$> (sequence $ parseFileSource <$> paths)
