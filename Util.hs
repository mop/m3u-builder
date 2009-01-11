module Util
where

import Maybe
import Data.List (find)

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
