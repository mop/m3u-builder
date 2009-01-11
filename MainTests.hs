module MainTests
    ( 
      prop_haveFilePath
    , prop_doubleLengthPlus1
    , prop_shouldParseCorrectly
    , prop_haveNotLastNL
    )
where

import Id3Parser
import Test.QuickCheck
import Main
import M3uParser

instance Arbitrary Char where
    arbitrary = elements (['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ 
                          "@!{}<>() -_")

instance Arbitrary Id3Info where
    arbitrary = do
        title <- arbitrary :: Gen String
        album <- arbitrary :: Gen String
        artist <- arbitrary :: Gen String
        year <- arbitrary :: Gen String
        genre <- arbitrary :: Gen String
        pop <- arbitrary :: Gen Integer

        return $ Id3Info title album artist year genre pop

filePathNotEmpty :: [(String, Id3Info)] -> Bool
filePathNotEmpty xs = all (\x -> length (fst x) > 0) xs

prop_doubleLengthPlus1 :: [(String, Id3Info)] -> Property
prop_doubleLengthPlus1 xs = filePathNotEmpty xs ==> 
        (length xs * 2 + 1) == length (lines $ toM3u xs)

prop_shouldParseCorrectly :: [(String, Id3Info)] -> Property
prop_shouldParseCorrectly xs = filePathNotEmpty xs ==> 
        (map fst xs) == m3uList
    where   m3u = toM3u xs
            m3uList = parseM3u' m3u

prop_haveFilePath :: [(String, Id3Info)] -> Property
prop_haveFilePath xs = filePathNotEmpty xs ==> 
                    foldr containsPath True xs
    where   str = toM3u xs
            m3uLines = lines str
            containsPath (path, _) flag | flag = path `elem` m3uLines
                                        | otherwise = False

prop_haveNotLastNL :: [(String, Id3Info)] -> Property
prop_haveNotLastNL xs = filePathNotEmpty xs ==> (last str) /= '\n'
    where   str = toM3u xs
