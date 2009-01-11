module Util
where

import Maybe
import Data.List (find)
import System.Directory (getHomeDirectory, doesDirectoryExist, createDirectory)
import Control.Monad (when)
import Control.Exception (try, evaluate)

import qualified Data.Map as M

import Id3Parser

{-
 - Looks the given list of mp3-files up in the given id3-map and returns a list
 - of Id3Info objects, which are included in the map.
 -}
m3uToid3List :: [String] -> M.Map String Id3Info -> [Id3Info]
m3uToid3List fps m = map fromJust filteredList
    where   maybeList = map (\x -> M.lookup x m) fps
            filteredList = filter isJust maybeList

{- 
 - Filters the entries in the similar list if they are contained in the given
 - map.
 -}
filterSimilar :: M.Map String Id3Info -> [Id3Info] -> [(String, Id3Info)]
filterSimilar m similar = map toMapArtist $ filter inMap similar
    where   list = map snd $ M.toList m
            inMap x = x `isIn` list
            isEql x y = id3Artist x == id3Artist y && id3Title x == id3Title y
            isIn x [] = False
            isIn x (y:ys) 
                | isEql x y = True
                | otherwise = x `isIn` ys
            toMapArtist x = fromJust $ find ((isEql x) . snd) $ M.toList m

{-
 - Loads the ID3-Cache from ~/.m3u-builder/cache
 -}
loadCache :: IO (M.Map String Id3Info)
loadCache = do
    home <- getHomeDirectory
    let dir  = home ++ "/.m3u-builder"
    let path = dir  ++ "/cache"
    result <- try (readFile path >>= return . read)
    case result of
        Left _  -> do
            exist <- doesDirectoryExist path
            when (not exist) (createDirectory dir)
            return M.empty
        Right m -> evaluate m

writeCache :: M.Map String Id3Info -> IO ()
writeCache cache = do
    home <- getHomeDirectory
    let dir = home ++ "/.m3u-builder"
    let path = dir ++ "/cache"
    result <- (try (writeFile path $ show cache))
    case result of 
        Left _ -> do
            exists <- doesDirectoryExist path
            if exists
                then    return ()
                else    createDirectory dir >> writeCache cache
        Right _ -> return ()
