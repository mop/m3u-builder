module LastFMTests
    (prop_parseArtists)
where

import Test.HUnit
import Test.QuickCheck
import Id3Parser
import LastFM

testShouldParseRegularList = TestCase $
    assertEqual "regular parsing test" 
        [testArtist] (parseToId3 $ 
                               "<lfm><similartracks>" ++ 
                               "<track><name>TestTitle</name>" ++
                               "<url>...</url>" ++
                               "<artist><name>TestArtist</name></artist>" ++
                               "</track>" ++
                               "</similartracks></lfm>")
    where testArtist = defaultId3Info { id3Artist = "TestArtist"
                                      , id3Title = "TestTitle"
                                      }

testShouldParseMultiList = TestCase $
    assertEqual "multi parsing test" 
        testArtists (parseToId3 $ 
                               "<lfm><similartracks>" ++ 
                               "<track><name>TestTitle1</name>" ++
                               "<url>...</url>" ++
                               "<artist><name>TestArtist1</name></artist>" ++
                               "</track>" ++
                               "<track><name>TestTitle2</name>" ++
                               "<url>...</url>" ++
                               "<artist><name>TestArtist2</name></artist>" ++
                               "</track>" ++
                               "</similartracks></lfm>")
    where testArtists = [
                defaultId3Info { id3Artist = "TestArtist1"
                               , id3Title = "TestTitle1"
                               }
              , defaultId3Info { id3Artist = "TestArtist2"
                               , id3Title = "TestTitle2"
                               }
              ]

testShouldParseEmptyList = TestCase $
    assertEqual "multi parsing test" 
        testArtists (parseToId3 $ 
                               "<lfm><similartracks>" ++ 
                               "</similartracks></lfm>")
    where testArtists = []

testShouldParseErrorList = TestCase $
    assertEqual "multi parsing test" 
        testArtists (parseToId3 $ 
                               "<lfm><similartracks>" ++ 
                               "</lfm>")
    where testArtists = []

tests = TestList [
    TestLabel "testShouldParseRegularList" testShouldParseRegularList
  , TestLabel "testShouldParseMultiList"   testShouldParseMultiList
  , TestLabel "testShouldParseEmptyList"   testShouldParseEmptyList
  , TestLabel "testShouldParseErrorList"   testShouldParseErrorList
  ]

main = runTestTT tests

instance Arbitrary Char where
    arbitrary = elements (['0' .. '9' ] ++ 
                          ['A' .. 'Z'] ++ 
                          ['a' .. 'z'] ++ 
                          "~!@$%^*() _-"
                         )
prop_parseArtists :: [(String, String)] -> Bool
prop_parseArtists artists = (parseToId3 xmlStr) == id3Artists
    where   xmlArtist (title, artist) = 
                "<track>" ++ 
                    "<name>" ++ title ++ "</name><url>..</url>" ++ 
                    "<artist><name>" ++ artist ++ "</name></artist>" ++ 
                "</track>"
            xmlStart = "<lfm><similartracks>"
            xmlEnd   = "</similartracks></lfm>"
            xmlStr = xmlStart ++ (concatMap xmlArtist artists) ++ xmlEnd
            id3Artists = map toId3 artists
            toId3 (title, artist) = defaultId3Info { id3Artist = artist
                                                   , id3Title = title 
                                                   } 
