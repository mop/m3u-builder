module Id3Parser
    ( Id3Info(..)
    , parseId3Info
    )
where

import System.IO.Unsafe

import Data.Bits (testBit, shiftL, (.|.))
import Data.Char (ord, chr, isPrint)
import Data.List (isInfixOf)
import Control.Monad.State
import Maybe

data Id3 = Id3 {
    id3Flags  :: Flag
  , id3Size   :: Int
  , id3Frames :: [Frame]
} deriving (Show)

data Flag = Flag {
    flagUnsynchronization     :: Bool
  , flagExtendedHeader        :: Bool
  , flagExperimentalIndicator :: Bool
} deriving (Show)

data Id3Info = Id3Info {
        id3Title :: String
      , id3Album :: String
      , id3Artist :: String
      , id3Year :: String
      , id3Genre :: String
      , id3Popularity :: Integer
} deriving (Show)

defaultFlag = Flag False False False

data Frame = Frame {
    frameId       :: String
  , frameSize     :: Int
  , frameContents :: String
} deriving (Show)

checkFlagUnsync :: Flag -> Int -> Flag
checkFlagUnsync f i   | testBit i 7 = f { flagUnsynchronization = True }
                      | otherwise   = f
checkFlagExtended :: Flag -> Int -> Flag
checkFlagExtended f i | testBit i 6 = f { flagExtendedHeader = True }
                      | otherwise = f
checkFlagExp :: Flag -> Int -> Flag
checkFlagExp f i      | testBit i 5 = f { flagExperimentalIndicator = True }
                      | otherwise = f

parseFlag :: Char -> Flag
parseFlag a = foldr setFlag defaultFlag [ checkFlagUnsync
                                        , checkFlagExtended
                                        , checkFlagExp
                                        ]
    where   i = ord a
            setFlag fun f = fun f i

data Id3Parser a = Id3Parser { runId3Parser :: String -> (a, String) }
instance Monad Id3Parser where
    return x = Id3Parser $ \s -> (x, s)
    m >>= k = Id3Parser $ \s -> let (a', s') = runId3Parser m s
                                in runId3Parser (k a') s'

parseChar :: Id3Parser Char
parseChar = Id3Parser $ \(x:xs) -> (x, xs)

parseChars :: Integer -> Id3Parser [Char]
parseChars 0 = return []
parseChars x = parseChar >>= \c -> parseChars (x - 1) >>= \cs -> return (c:cs)

parseFlags :: Id3Parser Flag
parseFlags = parseChar >>= return . parseFlag

parseFrame :: Id3Parser Frame
parseFrame = do
        id <- parseChars 4
        size <- parseInt
        fl <- parseChars 2
        content <- parseChars (toInteger $ size)
        return $ Frame id size content
        
parseInt :: Id3Parser Int
parseInt = parseChars 4 >>= return . toOktet (0 :: Int)
    where toOktet n (x:xs) = (shiftL (ord x) $ 8 * (length xs)) .|.
                             (toOktet n xs)
          toOktet n [] = n

parseSize :: Id3Parser Int
parseSize = chars >>= (return . (toSizeOktet (0 :: Int)))
    where chars = parseChars 4
          toSizeOktet :: Int -> [Char] -> Int
          toSizeOktet n (x:xs) = (shiftL (ord x) $ 7 * (length xs)) .|.
                                 (toSizeOktet n xs)
          toSizeOktet n [] = n

parseId3 :: Id3Parser Id3
parseId3 = do 
    fl <- parseFlags
    sz <- parseSize
    frames <- rekurFrames sz
    return (Id3 fl sz frames)
    where       rekurFrames :: Int -> Id3Parser [Frame]
                rekurFrames sz | sz > 10 = do
                                fr  <- parseFrame
                                frs <- rekurFrames (sz - frameSize fr - 10)
                                return $ fr : frs
                               | otherwise = return []
                               
parseId3Info :: String -> Maybe Id3Info
parseId3Info str = parseId3Base str >>= toId3Info
        where   toId3Info (id3,_) = liftM Id3Info getTitle 
                                         `ap` getAlbum 
                                         `ap` getArtist
                                         `ap` getYear
                                         `ap` getGenre
                                         `ap` getPopularity
                    where   getTitle = lookupContent id3 "TIT2"
                            getArtist = lookupContent id3 "TPE1"
                            getAlbum = lookupContent id3 "TALB"
                            getYear = lookupContent id3 "TYER"
                            getGenre = lookupContent id3 "TCON"
                            getPopularity = lookupPopularity id3

lookupPopularity id3 = (lookupFrame id3 "POPM" >>= return . fetchPopularity) 
                        `mplus` return 0
        where   fetchPopularity frm = toInteger . ord . head $ 
                                      dropWhile (\x -> ord x /= 0) 
                                        (frameContents frm) 

lookupFrame :: Id3 -> String -> Maybe Frame
lookupFrame id3 tag = contentFrame
        where   frames = id3Frames id3
                filteredFrames = filter ((tag ==) . frameId) frames
                contentFrame | length filteredFrames == 0 = Nothing
                             | otherwise = Just $ head filteredFrames
                            
lookupContent :: Id3 -> String -> Maybe String 
lookupContent id3 tag = ( lookupFrame id3 tag >>= 
                                (return . decode . frameContents)
                        ) `mplus` return ""

parseId3Base ('I':'D':'3':_:_:str) = Just $ 
    let (id3, str') = runId3Parser parseId3 (str)
    in (id3, take 10 str')
parseId3Base _ = Nothing

decode :: [Char] -> [Char]
decode ('\SOH':_:_:xs) = map encodeChars (groupList $ '\NUL':xs)
        where   encodeChars (h, l) = chr (0 .|. shiftL (ord h) 8 .|. ord l)
decode ('\NUL':xs) = filter isPrint xs
decode xs = xs

groupList :: [a] -> [(a, a)]
groupList [] = []
groupList (x:[]) = []
groupList (x:y:xs) = (x, y) : groupList xs

testIt = readFile "/home/nax/Musik/Amy MacDonald-This Is The Life/01  Amy MacDonald - Mr. Rock And Roll.mp3" >>= return . fromJust . parseId3Info

