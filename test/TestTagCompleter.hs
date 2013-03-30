{-# LANGUAGE OverloadedStrings #-}

module TestTagCompleter (tests) where

import           Control.Monad     (when)
import           Data.Aeson
import           Data.Aeson.Types
import           Data.HashMap.Lazy (fromList)
import           Data.Maybe        (fromJust, isNothing)
import qualified Data.Vector as V
import           Test.HUnit
import           Tagger.Types
import           Tagger.TagCompleter

import           Test.Framework.Providers.HUnit
import           Test.HUnit

tests = [ testCase "completeTrack" completeTrack
        , testCase "completeAlbum" completeAlbum
        , testCase "completeAll" completeAll
        , testCase "callService" callService
        , testCase "parseAlbum" parseAlbum
        ]

completeTrack = let track = Track "file" "loc" Nothing Nothing
                    album = Album "Nihility" [track] (Just 2007) (Just "Death Metal")
                    artist = Artist "Decapitated" [album]
                in do [dec] <- complete [artist]
                      artName dec @?= "Decapitated"
                      let nih = head $ filter (\a -> albName a == "Nihility") (artAlbs dec)
                      albName nih @?= "Nihility"
                      albRelease nih @?= Just 2007
                      albGenre nih @?= Just "Death Metal"
                      let tracks = albTracks nih
                      length tracks @?= 1
                      let t1 = head tracks
                      location t1 @?= "loc"
                      file t1 @?= "file"
                      name t1 @?= Just "Perfect Dehumanisation (The Answer)"
                      rank t1 @?= Just 1

completeAlbum = let track = Track "file" "loc" (Just "Perfect Dehumanisation") (Just 1)
                    album = Album "Nihility" [track] Nothing Nothing
                    artist = Artist "Decapitated" [album]
                in do [dec] <- complete [artist]
                      artName dec @?= "Decapitated"
                      let nih = head $ filter (\a -> albName a == "Nihility") (artAlbs dec)
                      albName nih @?= "Nihility"
                      albRelease nih @?= Just 2007
                      albGenre nih @?= Just "Death Metal"
                      let tracks = albTracks nih
                      length tracks @?= 1
                      let t1 = head tracks
                      location t1 @?= "loc"
                      file t1 @?= "file"
                      name t1 @?= Just "Perfect Dehumanisation"
                      rank t1 @?= Just 1

completeAll = let track = Track "file" "loc" Nothing Nothing
                  album = Album "Nihility" [track] Nothing Nothing
                  artist = Artist "Decapitated" [album]
              in do [dec] <- complete [artist]
                    artName dec @?= "Decapitated"
                    let nih = head $ filter (\a -> albName a == "Nihility") (artAlbs dec)
                    albName nih @?= "Nihility"
                    albRelease nih @?= Just 2007
                    albGenre nih @?= Just "Death Metal"
                    let tracks = albTracks nih
                    length tracks @?= 1
                    let t1 = head tracks
                    location t1 @?= "loc"
                    file t1 @?= "file"
                    name t1 @?= Just "Perfect Dehumanisation (The Answer)"
                    rank t1 @?= Just 1

callService = let artist = Artist "Immortal" [album]
                  album = Album "At the Heart of Winter" [] Nothing Nothing
              in do res <- query "Immortal" "At the Heart of Winter"
                    when (isNothing res) (assertFailure "no response from service")
                    let alb = fromJust res
                    length (albTracks alb) @?= 6
                    albRelease alb @?= Just 2007
                    albGenre alb @?= Just "Black Metal"
                    let t1 = head $ albTracks alb
                    name t1 @?= Just "Withstand the Fall of Time"
                    rank t1 @?= Just 1
                    let t6 = (albTracks alb) !! 5
                    name t6 @?= Just "Years of Silent Sorrow"
                    rank t6 @?= Just 6

parseAlbum = do let rlalb = parseEither parseJSON last_album
                when (isLeft rlalb) (assertFailure $ fromLeft rlalb)
                let alb = fromRight rlalb
                albName alb @?= "Descend Into Depravity"
                albRelease alb @?= Just 2009
                albGenre alb @?= Just "Death Metal"
                length (albTracks alb) @?= 2
                let t1 = head (albTracks alb)
                name t1 @?= Just "Your Treachery Will Die With You"
                rank t1 @?= Just 1
                let t2 = (albTracks alb) !! 1
                name t2 @?= Just "Ethos of Coercion"
                rank t2 @?= Just 8
               where isLeft (Left _ ) = True
                     isLeft _ = False
                     fromLeft (Left x) = x
                     fromRight (Right x) = x

last_tracks :: Value
last_tracks = Array (V.fromList [Object $ fromList [
               ("name",String "Your Treachery Will Die With You")
               ,("url",String "http://www.last.fm/music/Dying+Fetus/_/Your+Treachery+Will+Die+WIth+You")
               ,("artist",Object (fromList [ ("name",String "Dying Fetus") , ("url",String "http://www.last.fm/") , ("mbid",String "f76167bb-c117-4022-8b6b-54c796edf5c9") ]))
               ,("mbid",String "68e9067d-1da5-45d6-a76d-d1d609af6dff")
               ,("@attr",Object (fromList [ ("rank",String "1") ]))
               ,("duration",String "214")
               ,("streamable",Object (fromList [ ("#text",String "1") , ("fulltrack",String "1")]))]
              ,Object $ fromList [
                ("name",String "Ethos of Coercion")
               ,("url",String "http://www.last.fm/music/Dying+Fetus/_/Ethos+of+Coercion")
               ,("artist",Object $ fromList [ ("name",String "Dying Fetus") ,("url",String "http://www.lying+Fetus") ,("mbid",String "f76167bb-c117-4022-8b6b-54c796edf5c9")])
               ,("mbid",String "1dd30bea-e8f6-4767-9c12-250a3549caf9")
               ,("@attr",Object $ fromList [ ("rank",String "8")])
               ,("duration",String "196")
               ,("streamable",Object $ fromList [ ("#text",String "1") ,("fulltrack",String "1")])]])

last_album :: Value
last_album = Object $ fromList [
    ("album",Object $ fromList [
        ("wiki",Object $ fromList [
            ("published",String "Sat, 19 Dec 2009 03:15:17 +0000")
           ,("summary",String "Descend into Depravity is the sixth studio album by  class=\"bbcode_artist\">Dying Fetus</a>, released on September 15, 2009.")
           ,("content",String "<strong><em>Descend into Depravity</em></strong> is the sixth studio album by SA License and may also be available under the GNU FDL.")
            ])
      ,("playcount",String "649071")
      ,("name",String "Descend Into Depravity")
      ,("url",String "http://www.last.fm/music/Dying+Fetus/Descend+Into+Depravity")
      ,("artist",String "Dying Fetus")
      ,("tracks", Object $ fromList [ ("track",last_tracks) ])
      ,("mbid",String "4aa2f613-0478-44ba-aa35-28230a3e2456")
      ,("id",String "77974816")
      ,("listeners",String "37944")
      ,("image",Array (V.fromList [
          Object $ fromList [("size",String "small") ,("#text",String "http://userserve-ak.last.fm/serve/34s/63608935.png")]
         ,Object $ fromList [("size",String "medium") ,("#text",String "http://userserve-ak.last.fm/serve/64s/63608935.png")]
         ,Object $ fromList [("size",String "large") ,("#text",String "http://userserve-ak.last.fm/serve/174s/63608935.png")]
         ,Object $ fromList [("size",String "extralarge") ,("#text",String "http://userserve-ak.last.fm/serve/300x300/63608935.png")]
         ,Object $ fromList [("size",String "mega") ,("#text",String "http://userserve-ak.last.fm/serve/_/63608935/Descend+Into+Depravity+High+quality+PNG.png")]]))
      ,("releasedate",String "    15 Sep 2009, 00:00")
      ,("toptags",Object $ fromList [
            ("tag",Array (V.fromList [
                Object $ fromList [
                    ("name",String "death metal")
                   ,("url",String "http://www.last.fm/tag/death%20metal")]
               ,Object $ fromList [
                    ("name",String "brutal death metal")
                   ,("url",String "http://www.last.fm/tag/brutal%20death%20metal")]
               ,Object $ fromList [
                    ("name",String "technical death metal")
                   ,("url",String "http://www.last.fm/tag/technical%20death%20metal")]
               ,Object $ fromList [
                    ("name",String "2009")
                   ,("url",String "http://www.last.fm/tag/2009")]
               ,Object $ fromList [("name",String "metal"),("url",String "http://www.last.fm/tag/metal")]]))])
      ])]
