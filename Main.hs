module Main (main) where

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
import JarFind

-- Types related to cmdline arguments
 
data ClassFileSource = ClassFile { path :: FilePath }
                     | JarFile   { path :: FilePath }
                     | ClassPath { paths :: [ClassFileSource] }

data Location = InFile FilePath
              | InJar { jarPath :: FilePath
                      , innerPath :: FilePath
                      }
                deriving Show

newtype SearchSource = SearchSource (Class->Bool)
data SearchTarget = SearchClass
                  | SearchMember (Member->Bool)

data Args = Args { dataSource   :: [ClassFileSource]
                 , searchSource :: SearchSource
                 , searchTarget :: SearchTarget
                 }

data Result = FoundClass Location Class
            | FoundMember Location Class Member
              deriving Show


----------------------------------------------------------------
--                    ARGUMENTS PARSING                       --
----------------------------------------------------------------


data PlainArgs = PlainArgs { typeRegex :: Maybe String
                           , memberRegex :: Maybe String
                           , typeAccess :: Maybe String
                           , memberAccess :: Maybe String
                           , target :: Maybe String 
                           }

emptyArgs :: PlainArgs
emptyArgs = PlainArgs Nothing Nothing Nothing Nothing Nothing

data MemberKind = FieldKind | MethodKind deriving Eq

parseArgs :: PlainArgs -> [String] -> Args
parseArgs a paths = Args dataSource (SearchSource typeFilter) searchTarget
    where searchTarget = case (memberRegex a, memberAccess a, target a) of 
                            (Nothing, Nothing, Nothing) -> SearchClass
                            _                           -> SearchMember memberFilter

          searchMembers = isJust (memberRegex a) 

          typeFilter = accessAndRegex (access (typeAccess a)) (typeRegex a) clsAccess clsName
          
          memberFilter mem = (memKind `maybeEq` kind mem)
                           && accessAndRegex (access (memberAccess a)) (memberRegex a) 
                                             mAccess mName mem
              where kind (Field  _ _ _) = FieldKind
                    kind (Method _ _ _) = MethodKind

          memKind = case (target a) of 
                        Just ('f':_) -> Just FieldKind 
                        Just ('m':_) -> Just MethodKind 
                        _            -> Nothing

          dataSource = map toDataSource paths
          toDataSource p | ".jar"   `isSuffixOf` p = JarFile p
                         | ".class" `isSuffixOf` p = ClassFile p
                         | ":"      `isInfixOf`  p = ClassPath . map toDataSource $ (==':') `unjoin` p
                         | otherwise               = error $ "Unsupported datasource type: "++p

accessAndRegex acc rx getAcc getName x =  (acc `maybeEq` (getAcc x)) 
                                       && (nameMatchesRegex x)
    where nameMatchesRegex x = case rx of 
                                  Nothing -> True
                                  Just rx -> getName x =~ rx
          
maybeEq Nothing  _ = True
maybeEq (Just a) b = a == b
          
access a = case a of
              Just "public"    -> Just Public
              Just "private"   -> Just Private
              Just "protected" -> Just Protected
              Just "package"   -> Just Package
              _                -> Nothing

unjoin :: (a -> Bool) -> [a] -> [[a]]
unjoin p s = go [] [] s
    where go res cur []     = reverse (cur:res)
          go res cur (x:xs) | p x       = go (cur:res) [] xs
                            | otherwise = go res (x:cur) xs



----------------------------------------------------------------
--                            MAIN                            --
----------------------------------------------------------------
main :: IO ()
main = do
    (opts, paths, errs) <- getOpt Permute options `liftM` getArgs
    let compose = foldr (.) id
    case errs of
        [] -> run (parseArgs (compose opts emptyArgs) paths)
        _  -> mapM_ putStrLn errs >> showHelp


options :: [OptDescr (PlainArgs -> PlainArgs)]
options = [ Option ['c'] []         (OptArg (\s a -> a { typeRegex    = s }) "RX")   
                "Search for/in types whose name (with package) contains a match of this regex",
            Option ['m'] []         (OptArg (\s a -> a { memberRegex  = s }) "RX")  
                "Search for members whose name contains a match of this regex",
            Option []    ["ca"]     (OptArg (\s a -> a { typeAccess   = s })"ACCESS")  
                "Search for/in types having the specified access (public/private/protected/package)",
            Option []    ["ma"]     (OptArg (\s a -> a { memberAccess = s }) "ACCESS") 
                "Search for members having the specified access (public/private/protected/package)",
            Option ['t'] ["target"] (OptArg (\s a -> a { target       = s }) "TYPE")   
                "Search for members of the given type (field=f/method=m)"]

showHelp :: IO ()
showHelp = putStrLn $ usageInfo usage options
    where usage="jarf [OPTION]... FILE... - Search for classes/methods/interfaces in JAR file(s)"

run :: Args -> IO ()
run (Args dataS searchS searchT) = do
    classes <- parseFileSource (ClassPath dataS)
    mapM_ (putStrLn . present) . concatMap (search searchS searchT) $ classes

-- Results presentation

present :: Result -> String
present (FoundClass loc cls) = presentLoc loc ++ ": " ++ presentClass cls
present (FoundMember loc cls mem) = presentLoc loc ++ ": " ++ 
                                    presentClass cls ++ ": " ++ 
                                    show (mAccess mem) ++ " " ++ 
                                    (mName mem) ++ " :: " ++ 
                                    (mSig mem)

presentLoc :: Location -> String
presentLoc (InFile path) = path
presentLoc (InJar jp ip) = jp ++ "!" ++ ip

presentClass :: Class -> String
presentClass (Class acc _ name False) = show acc ++ " class "     ++ name
presentClass (Class acc _ name True)  = show acc ++ " interface " ++ name

-- Search

search :: SearchSource -> SearchTarget -> (Location,Class) -> [Result]
search (SearchSource classP) SearchClass (loc,c) = 
    if (classP c) then [FoundClass loc c] else []
search (SearchSource classP) (SearchMember memP) (loc,c) =
    if (classP c) then [FoundMember loc c mem | mem <- clsMembers c, memP mem]
                  else []


-- Getting the actuall classes to search in

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
