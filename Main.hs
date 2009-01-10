{-# LANGUAGE MultiParamTypeClasses #-}
import System (getArgs, exitWith, ExitCode(..))
import System.Directory (doesDirectoryExist, getDirectoryContents)
import Control.Monad (filterM, when, foldM)
import Data.List (isSuffixOf, (\\))
import Control.Monad.Trans

import qualified Data.Map as M

import Id3Parser

type Id3Map = M.Map String Id3Info

newtype FileWriter a = FileWriter { runFW :: (a, [String]) }
instance Monad FileWriter where
    return x = FileWriter (x, [])
    m >>= k = FileWriter $ 
                let (a, s)   = runFW m
                    (a', s') = runFW (k a)
                in (a', s' ++ s)

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
    

findMp3Files :: FilePath -> IO [String]
findMp3Files path = runFWT (findMp3Files' path) >>= return . snd 

parseId3' :: FilePath -> IO (Maybe Id3Info)
parseId3' fp = readFile fp >>= (return . parseId3Info)

buildMap :: FilePath -> IO Id3Map
buildMap fp = findMp3Files fp >>= (foldM buildMap' M.empty)
    where   buildMap' map file = do
                id3 <- parseId3' file
                maybe (return map) (\x -> return (M.insert file x map)) id3

main = do
    args <- getArgs
    case args of
        (dir:playlist:xs) -> do
            map <- buildMap dir
            putStrLn $ show map
        otherwise -> do
            putStrLn "Error, usage: recommendator <Music-Directory> <Playlist"
            exitWith $ ExitFailure 1
