module TestCrawler (tests) where

import Control.Applicative   ((<$>))
import Control.Monad         (when)
import System.Exit           (exitFailure)
import System.FilePath.Posix ((</>))
import Tagger.Types
import Tagger.Crawler

import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.QuickCheck
import TestUtils                            (getDataDir)

tests = [ testCase "crawling files" crawlin
        , testCase "fillTypes" fillTypes
        , testProperty "add prefix" prop_ismp3_prefix
        , testProperty "add prefix 2" prop_ismp3_prefix'
        , testProperty "add suffix" prop_ismp3_suffix
        ]

newtype File = File String deriving (Show)

instance Arbitrary File where
    arbitrary = do
        prefix <- arbitrary :: Gen String
        suffix <- elements ["mp3", "jpg", "mp4", "", "conf"]
        return (File $ prefix ++ "." ++ suffix)

prop_ismp3_prefix file = isMp3 (unwrap file) ==> isMp3 ("foo." ++ (unwrap file)) 
    where unwrap (File x) = x

prop_ismp3_prefix' file = isMp3 (unwrap file) ==> isMp3 ("foo" ++ (unwrap file)) 
    where unwrap (File x) = x

prop_ismp3_suffix file = isMp3 (unwrap file) ==> not . isMp3 $ (unwrap file) ++ "foo"
    where unwrap (File x) = x

crawlin = do
    dir <- getDataDir
    artists <- listArtists dir
    length artists @?= 3

    let decap = head $ filter (\a -> artName a == "Decapitated") artists
    length (artAlbs decap) @?= 1
    let nih = head $ filter (\a -> albName a == "Nihility") (artAlbs decap)
    length (albTracks nih) @?= 3

    let hate = head $ filter (\a -> artName a == "Hate Eternal") artists
    length (artAlbs hate) @?= 1
    let fur = head $ filter (\a -> albName a == "Fury & Flames") (artAlbs hate)
    length (albTracks fur) @?= 3

fillTypes = do dir <- getDataDir
               artists <- listArtists dir
               let agr = head $ filter (\a -> artName a == "Agrypnie") artists
               length (artAlbs agr) @?= 2
               let ext = head $ filter (\a -> albName a == "Exit") (artAlbs agr)
               length (albTracks ext) @?= 3
               let t1 = head $ filter (\t -> file t == "1-Mauern.mp3") (albTracks ext)
               location t1 @?= (dir </> "Agrypnie" </> "Exit")
               let t2 = head $ filter (\t -> file t == "2-Gedanken.mp3") (albTracks ext)
               location t2 @?= (dir </> "Agrypnie" </> "Exit")
               let t3 = head $ filter (\t -> file t == "3-Zorn.mp3") (albTracks ext)
               location t3 @?= (dir </> "Agrypnie" </> "Exit")
