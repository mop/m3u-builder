module Song
where

import Time

data Song = Song {
    songName   :: String
  , songLength :: Integer
  , songGenre  :: String
} deriving (Show, Eq, Read)

data Album = Album {
    albumName  :: String
  , albumSongs :: [Song]
  , albumReleased :: CalendarTime
} deriving (Show, Eq, Read)

data Artist = Artist {
    artistName   :: String
  , artistAlbums :: [Album]
} deriving (Show, Eq, Read)

