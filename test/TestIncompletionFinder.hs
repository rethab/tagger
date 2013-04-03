module TestIncompletionFinder (tests) where

import Control.Applicative ((<$>))
import Data.Maybe          (isNothing)
import System.FilePath
import Tagger.Types
import Tagger.IncompletionFinder
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck
import TestUtils

tests = [ testCase "fillTags" fillTags
        , testProperty "incompleteArtist" prop_art_incomp]

prop_art_incomp art = incomp art == incoArt art
    where incoArt (Artist _ albs) | or (map incoAlb albs) = True
                                  | otherwise = False
          incoAlb (Album _ ts rel gen) | or (map incoTrk ts) = True
                                       | isNothing rel = True
                                       | isNothing gen = True
                                       | otherwise = False
          incoTrk (Track _ _ n r) | isNothing n = True
                                  | isNothing r = True
                                  | otherwise = False
                                    


fillTags = do
    dir <- getDataDir
    arts <- getIncomplete [decap_in dir, hate_in dir, agr_in dir]
    length arts @?= 2
    let hate = head $ filter (\a -> artName a == "Hate Eternal") arts
    length (artAlbs hate) @?= 1
    let fur = head $ filter (\a -> albName a == "Fury & Flames") (artAlbs hate)
    let tracks = albTracks fur
    length tracks @?= 3
    albRelease fur @?= Nothing
    albGenre fur @?= Nothing

    let t1s = filter (\t -> file t == "01 - Track 1.mp3") tracks
    assertEqual "track 1 missing" 1 (length t1s)
    let t1 = head t1s
    name t1 @?= Nothing
    rank t1 @?= Just 1

    let t2s = filter (\t -> file t == "02 - Track 2.mp3") tracks
    assertEqual "track 2 missing"  1 (length t2s)
    let t2 = head t2s
    name t2 @?= Just "Whom Gods May Destroy"
    rank t2 @?= Nothing

    let t3s = filter (\t -> file t == "03 - Track 3.mp3") tracks
    assertEqual "track 3 missing" 1 (length t3s)
    let t3 = head t3s
    name t3 @?= Just "Para Bellum"
    rank t3 @?= Just 3

-- after crawling
decap_in dir = Artist "Decapitated" [nih]
    where nih =  Album "Nihility" tracks Nothing Nothing
          tracks = [ Track "01 - Perfect Dehumanisation (The Answer_).mp3"
                           (dir </> "Decapitated/Nihility/")
                            Nothing Nothing
                   , Track "02 - Eternity Too Short.mp3"
                           (dir </> "Decapitated/Nihility")
                           Nothing Nothing
                   , Track "03 - Mother War.mp3"
                           (dir </> "Decapitated/Nihility")
                           Nothing Nothing
                   ]

agr_in dir = Artist "Agrypnie" [ext]
    where ext =  Album "Exit" tracks Nothing Nothing
          tracks = [ Track "1-Mauern.mp3"
                           (dir </> "Agrypnie/Exit")
                           Nothing Nothing
                   ]

hate_in dir = Artist "Hate Eternal" [fur]
    where fur =  Album "Fury & Flames" tracks Nothing Nothing
          tracks = [ Track "01 - Track 1.mp3"
                           (dir </> "Hate Eternal/Fury & Flames")
                           Nothing Nothing
                   , Track "02 - Track 2.mp3"
                           (dir </> "Hate Eternal/Fury & Flames")
                           Nothing Nothing
                   , Track "03 - Track 3.mp3"
                           (dir </> "Hate Eternal/Fury & Flames")
                           Nothing Nothing
                   ]

-- after filling in info from tags
decap_after dir = Artist "Decapitated" [nih]
    where nih = Album "Nihility" [nam] (Just 2002)
                
                             (Just "Technical Death Metal")
          nam = Track "05 - Names.mp3"
                      (dir </> "Decapitated/Nihility/")
                      (Just "Names") (Just 5)
