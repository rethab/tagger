module TestTagWriter (tests) where

import System.FilePath
import Test.HUnit
import Tagger.Types
import Tagger.Crawler
import Tagger.TagWriter

import TestUtils (getDataDir)
import Test.Framework.Providers.HUnit
import Test.HUnit

tests = [ testCase "oneTrack" oneTrack
        , testCase "albumOnly" albumOnly
        , testCase "writeBoth" writeBoth
        ]

oneTrack = do dir <- getDataDir 
              let artist = Artist "Agrypnie" [album]
                  album = Album "Exit" [track] Nothing Nothing
                  track = Track "1-Mauern.mp3" (dir </> "Agrypnie/Exit")
                          (Just "Mauern") (Just 1)
              writeTags [artist]
              artists <- listArtists dir
              let agr = head $ filter (\a -> artName a == "Agrypnie") artists
              let ext = head (artAlbs agr)
              let t1 = head $ filter (\t -> file t == "1-Mauern.mp3") (albTracks ext)
              name t1 @?= Just "Mauern"
              rank t1 @?= Just 1
                 
albumOnly = do dir <- getDataDir 
               let artist = Artist "Agrypnie" [album]
                   album = Album "Exit" [track] (Just 2008) (Just "Black Metal")
                   track = Track "2-Gedanken.mp3" (dir </> "Agrypnie/Exit")
                                Nothing Nothing
               writeTags [artist]
               artists <- listArtists dir
               let agr = head $ filter (\a -> artName a == "Agrypnie") artists
               let ext = head (artAlbs agr)
               albRelease ext @?= Just 2010
               albGenre ext @?= Just "Black Metal"

writeBoth = do dir <- getDataDir
               let track = Track "3-Zorn.mp3" (dir </> "Agrypnie/Exit")
                                (Just "Zorn") (Just 3)
                   artist = Artist "Agrypnie" [album]
                   album = Album "Exit" [track] (Just 2008) (Just "Black Metal")
               writeTags [artist]
               artists <- listArtists dir
               let agr = head $ filter (\a -> artName a == "Agrypnie") artists
               let ext = head (artAlbs agr)
               albRelease ext @?= Just 2010
               albGenre ext @?= Just "Black Metal"
               let t1 = head $ filter (\t -> file t == "3-Zorn.mp3") (albTracks ext)
               name t1 @?= Just "Mauern"
               rank t1 @?= Just 1
