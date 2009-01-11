module LastFM
    ( findSimilar
    , parseToId3
    )
where

import Network.HTTP
import Network.URI
import Control.Monad
import Control.Monad.Error
import Maybe

import Text.XML.HaXml
import Text.XML.HaXml.Parse

import Id3Parser

{-
 - The last.fm api key
 -}
apiKey = "8bfc6ab7a6d8961b081d3df495f37f47"

{-
 - Filters the Just-elements in the given list and returns the encapsulated
 - values from the list.
 -}
filterJust = (map fromJust) . (filter isJust)

{-
 - Finds similar songs for the given id3 info struct and returns them.
 -}
findSimilar :: [Id3Info] -> IO [[Id3Info]]
findSimilar infos = do
            cs <- contents 
            return $ map parseToId3 (filterJust cs)
    where   contents :: IO [Maybe String]
            contents = mapM (downloadURL . mkUrl) infos

{-
 - Creates a url out of the given id3 struct which is used for lookup on
 - last.fm
 -}
mkUrl :: Id3Info -> String
mkUrl id3 = base ++ method ++ artist ++ track ++ apiKey'
    where   base    = "http://ws.audioscrobbler.com/2.0/"
            method  = "?method=track.getsimilar"
            artist  = "&artist="  ++ (urlEncode (id3Artist id3))
            track   = "&track="   ++ (urlEncode (id3Title id3))
            apiKey' = "&api_key=" ++ apiKey

{-
 - Downloads the contents of the given url.
 -}
downloadURL :: String -> IO (Maybe String)
downloadURL url = maybe (return Nothing) doRequest request
    where   request = liftM4 Request   uri 
                                      (return GET) 
                                      (return [])
                                      (return "")
            doRequest req = do 
                result <- simpleHTTP req
                return $ either (const Nothing) (Just . rspBody) result
            uri = parseURI url

{-
 - Parses an xml-string fetched from last.fm and converts it to an
 - Id3Info-struct
 -
 - @content: a string which looks like:
 -  <lfm status="ok">
 -    <similartracks track="Believe" artist="Chor">
 -      <track>
 -        <name>Ray of Light</name>
 -        <playcount/>
 -        <mbid/>
 -        <match>10.94</match>
 -        <url>...</url>
 -        <streamable fulltrack="0">1</streamable>
 -        <duration>24000</duration.
 -        <artist>
 -          <name>Madonna</name>
 -          <mbid>....</mbid>
 -          <url>..</url>
 -        </artist>
 -      </track>
 -      <track>
 -      ...
 -      </track>
 -      ...
 -    </similartracks>
 -  </lfm>
 -}
parseToId3 :: String -> [Id3Info]
parseToId3 content = either (const []) id (doc >>= return . getId3Infos)
    where   doc :: Either String Content
            doc = parseResult >>= getContent
            parseResult = xmlParse' "-" content

            getContent :: Document -> Either String Content
            getContent (Document _ _ e _) = return (CElem e)

{-
 - Parses the given xml-content
 -}
getId3Infos :: Content -> [Id3Info]
getId3Infos doc = map toId3Info tracks'
    where   tracks' = tracks doc
            toId3Info content = defaultId3Info { id3Title = getTrack
                                               , id3Artist = getArtist
                                               }
                where   getTrack  = contentToString (trackName content)
                        getArtist = contentToString (artistName content)

similartracks :: CFilter
similartracks = tag "lfm" /> tag "similartracks"

tracks :: CFilter
tracks = similartracks /> tag "track"

trackName :: CFilter
trackName = keep /> tag "name" /> txt

artistName :: CFilter
artistName = keep /> tag "artist" /> tag "name" /> txt

contentToString :: [Content] -> String
contentToString doc = concatMap procContent doc
    where   procContent (CString _ str) = str
            procContent x = ""
