{-# LANGUAGE DataKinds, OverloadedStrings #-}

module Tagger.TagCompleter
    ( complete
    , getAlbInfo
    , query
    ) where

import           Control.Applicative  ((<$>), (<*>))
import           Control.Monad        (mapM, mzero)
import           Data.Aeson.Types
import qualified Data.Char            as C
import           Data.Maybe           (isNothing, maybe)
import           Data.Text            as T
import           Network.Lastfm       (lastfm, (<*), artist, album, apiKey,
                                       json, Response, Format(JSON))
import qualified Network.Lastfm.Album as LastAlbum
import           Prelude              as P
import           Tagger.Types

taggerapikey = "a2c21e95ab7239f87f2e5ff716fc6374"

complete :: [Artist] -> IO [Artist]
complete arts = mapM completeArt arts

completeArt :: Artist -> IO Artist
completeArt art = mapM (completeAlb art) (artAlbs art) >>= \albs ->
                    return $ art { artAlbs = albs }

completeAlb :: Artist -> Album -> IO Album
completeAlb art alb = query (artName art) (albName alb) >>=
                        return . maybe alb (addATags alb)

addATags :: Album -> Album -> Album
addATags orig last = orig { albTracks = P.zipWith addTTags (albTracks orig) (albTracks last)
                          , albRelease = release
                          , albGenre = genre }
    where release = if isNothing (albRelease orig)
                        then albRelease last
                        else albRelease orig
          genre   = if isNothing (albGenre orig)
                        then albGenre last
                        else albGenre orig

addTTags :: Track -> Track -> Track
addTTags orig last = orig { name = name', rank = rank' }
    where name' = if isNothing (name orig)
                      then name last
                      else name orig
          rank' = if isNothing (rank orig)
                      then rank last
                      else rank orig

query :: String -> String -> IO (Maybe Album)
query art alb = do resp <- getAlbInfo (T.pack art) (T.pack alb)
                   return (resp >>= parseMaybe parseJSON)

getAlbInfo :: Text -> Text -> IO (Response JSON)
getAlbInfo art alb = lastfm $ LastAlbum.getInfo
                        <*> artist art
                        <*> album alb
                        <*> apiKey taggerapikey
                        <*  json

instance FromJSON Track where
    parseJSON (Object v) = do
        name <- v .: "name"
        atts <- v .: "@attr"
        rank <- atts .: "rank"
        return (Track "" "" (Just name)  (Just $ read rank))
    parseJSON _ = mzero

instance FromJSON Album where
    parseJSON (Object v) = do
        alb <- v .: "album"
        title <- alb .: "name"
        tracks' <- alb .: "tracks"
        tracks <- tracks' .: "track"
        release <- parseDate <$> (alb .: "releasedate")
        genre'' <- alb .: "toptags"
        genre' <- P.head <$> (genre'' .: "tag")
        genre <- capitalize <$> genre' .: "name"
        return (Album title tracks (Just release) (Just genre))
    parseJSON _ = mzero

parseDate :: String -> Int
parseDate = P.read . P.take 4 . (!! 2) . P.words

capitalize :: Text -> String
capitalize = P.unwords . P.map up . P.words . T.unpack
    where up (x:xs) = C.toUpper x : xs
