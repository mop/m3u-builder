module M3uParser
    (
      parseM3u
    , parseM3u'
    )
where

{-
 - Parses the m3u file.
 -}
parseM3u :: String -> IO [String]
parseM3u file = readFile file >>= (return . (map removeCR) . parseM3u')

parseM3u' :: String -> [String]
parseM3u' = (filter (('#' /=) . head)) . lines  

removeCR xs | last xs == '\r' = take (length xs - 1) xs
removeCR xs | otherwise       = xs
