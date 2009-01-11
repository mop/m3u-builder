{-# LANGUAGE MultiParamTypeClasses #-}
module Main
    (toM3u)
where

import System (getArgs, exitWith, ExitCode(..))
import System.Directory (doesDirectoryExist, getDirectoryContents)
import Control.Monad (filterM, when, foldM, mplus)
import Data.List (isSuffixOf, (\\), nub)
import Control.Monad.Trans
import Control.Exception (evaluate)

import qualified Data.Map as M

import Id3Parser
import M3uParser
import Util
import LastFM

type Id3Map = M.Map String Id3Info

{-
 - A writer monad which is used to log all mp3 files
 -}
newtype FileWriter a = FileWriter { runFW :: (a, [String]) }
instance Monad FileWriter where
    return x = FileWriter (x, [])
    m >>= k = FileWriter $ 
                let (a, s)   = runFW m
                    (a', s') = runFW (k a)
                in (a', s' ++ s)

{-
 - A writer monad transformer which is used to log all mp3 files in the
 - file-system.
 -}
newtype FileWriterT m a = FileWriterT { runFWT :: m (a, [String]) }
instance (Monad m) => Monad (FileWriterT m) where
    return x = FileWriterT $ return (x, [])
    m >>= k = FileWriterT $ do
                (a, s)   <- runFWT m 
                (a', s') <- runFWT (k a)
                return (a', s' ++ s)

instance MonadTrans FileWriterT where
    lift m = FileWriterT $ m >>= \x -> return (x, [])

class (Monad m) => FileWriterClass m where
    writeFileWriter :: String -> m ()

instance FileWriterClass FileWriter where
    writeFileWriter f = FileWriter ((), [f])

instance (Monad m) => FileWriterClass (FileWriterT m) where
    writeFileWriter f = FileWriterT $ return ((), [f])

{-
 - Finds recursively all mp3 files and writes them to the FileWriterT monad.
 -}
findMp3Files' :: FilePath -> FileWriterT IO ()
findMp3Files' path = do
    contents <- lift $ getDirectoryContents path >>= 
        (return . (map ((path ++ "/") ++) . filter (`notElem` [".", ".."])))
    when (null contents)
        (return ())
    dirs <- lift $ filterM doesDirectoryExist contents
    let files = filter (".mp3" `isSuffixOf`) $ contents \\ dirs
    mapM_ writeFileWriter files
    mapM_ findMp3Files' dirs
    

{- 
 - Finds all mp3 files recursively in the given path.
 -}
findMp3Files :: FilePath -> IO [String]
findMp3Files path = runFWT (findMp3Files' path) >>= return . snd 

{-
 - Parses the given MP3 file and returns the id3 info from the file.
 -}
parseId3' :: FilePath -> IO (Maybe Id3Info)
parseId3' fp = do 
        cs <- readFile fp 
        evaluate (parseId3Info cs `mplus` parseId3v1Info cs)

{-
 - Searches the file-system for mp3-files, parses their id3 tags and creates a
 - hash which maps their file-path to the id3 tag.
 -}
buildMap :: FilePath -> IO Id3Map
buildMap fp = findMp3Files fp >>= (foldM buildMap' M.empty)
    where   buildMap' map file = do
                id3 <- parseId3' file
                maybe (return map) (\x -> return $! (M.insert file x map)) id3

{-
 - Removes the trailing slash ('/') from the given string
 -}
unSlash xs | last xs == '/' = take (length xs - 1) xs
           | otherwise      = xs


{-
 - Converts the given id3 information to an m3u file
 -}
toM3u :: [(String, Id3Info)] -> String
toM3u = (header ++) . (concatMap id3ToM3u) . (addIndex [1..])
    where   header = "#EXTM3U\n"
            id3ToM3u (id, path, id3) = "#EXTINF:100," ++ 
                                       (show id) ++ 
                                       " " ++ id3Artist id3 ++ " - " ++
                                       id3Title id3 ++ " (" ++ id3Album id3 ++
                                       " )\n" ++ path ++ "\n"
            addIndex :: [Integer] -> [(String, Id3Info)] -> 
                        [(Integer, String, Id3Info)]
            addIndex (y:ys) ((f, id3):xs) = (y, f, id3) : (addIndex ys xs)
            addIndex _ _ = []

main = do
    args <- getArgs
    case args of
        (dir:playlist:xs) -> do
            m <- buildMap (unSlash dir)
            m3uList <- (parseM3u playlist >>= (return . (flip m3uToid3List m)))
            similar <- findSimilar m3uList
            let results = filterSimilar m (nub (concat similar))
            putStrLn $ toM3u results
        otherwise -> do
            putStrLn "Error, usage: recommendator <Music-Directory> <Playlist"
            exitWith $ ExitFailure 1
