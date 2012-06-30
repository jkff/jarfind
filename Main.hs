{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Codec.Archive.Zip
import qualified Data.ByteString.Lazy as B
import Control.Monad
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
    classes <- parseDataSource dataS
    mapM_ (putStrLn . present) . concatMap (search searchS searchT) $ classes

-- Results presentation

present :: Result -> String
present (FoundClass loc cls) = presentLoc loc ++ ": " ++ presentClass cls
present (FoundMember loc cls mem) = presentLoc loc ++ ": " ++ 
                                    presentClass cls ++ ": " ++ 
                                    show (mAccess mem) ++ " " ++ 
                                    bToString (mName mem) ++ " :: " ++ 
                                    bToString (mSig mem)

presentLoc :: Location -> String
presentLoc (InFile path) = path
presentLoc (InJar jp ip) = jp ++ "!" ++ ip

presentClass :: Class -> String
presentClass (Class acc _ name False) = show acc ++ " class "     ++ bToString name
presentClass (Class acc _ name True)  = show acc ++ " interface " ++ bToString name

-- Search

search :: SearchSource -> SearchTarget -> (Location,Class) -> [Result]
search (SearchSource classP) SearchClass (loc,c) = 
    if (classP c) then [FoundClass loc c] else []
search (SearchSource classP) (SearchMember memP) (loc,c) =
    if (classP c) then [FoundMember loc c mem | mem <- clsMembers c, memP mem]
                  else []


-- Getting the actuall classes to search in

parseDataSource :: [ClassFileSource] -> IO [(Location, Class)]
parseDataSource = fmap (map (second parseClassFile)) . fmap concat . sequence . map files
    where second f (a,b) = (a, f b)

files :: ClassFileSource -> IO [(Location, B.ByteString)]
files (ClassFile path) = do
    f <- B.readFile path
    return [(InFile path, f)]
files (JarFile jar)    = do
    archive <- toArchive `liftM` B.readFile jar
    return [(InJar jar fileName, fromEntry entry)
            | entry <- zEntries archive,
              let fileName = eRelativePath entry,
              ".class" `isSuffixOf` fileName]
files (ClassPath paths) = fmap concat . sequence . map files $ paths



