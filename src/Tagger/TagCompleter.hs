{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Tagger.TagCompleter where

-- from base
import           Control.Applicative  ((<$>), (<*>))
import           Control.Exception    (SomeException, try)
import           Control.Monad        (mzero)
import qualified Data.Char            as C
import           Data.Either          (partitionEithers)
import           Data.Maybe           (isNothing)
import           Prelude              as P

-- from text
import           Data.Text            as T

-- from liblastfm
import qualified Network.Lastfm.Album as LastAlbum
import           Network.Lastfm       (lastfm, (<*), artist, album, apiKey,
                                       json, Response, Format(JSON))

-- from aeson
import           Data.Aeson.Types

import           Tagger.Types

taggerapikey :: Text
taggerapikey = "a2c21e95ab7239f87f2e5ff716fc6374"

complete :: [Artist] -> IO [Either String Artist]
complete arts = mapM completeArt arts

-- | Completes on Artist's tag information based on last.fm sources. If
-- one of the album fails, completion of the entire artist fails and
-- individual error messages are concatenated.
completeArt :: Artist -> IO (Either String Artist)
completeArt art = do etagged <- mapM (completeAlb art) (artAlbs art)
                     case partitionEithers etagged of
                        ([], albs) -> return (Right $ art { artAlbs = albs })
                        (errs, _) -> return (Left $ P.unlines errs)

completeAlb :: Artist -> Album -> IO (Either String Album)
completeAlb art alb = do etags <- query (artName art) (albName alb)
                         return (etags >>= Right . addATags alb)

addATags :: Album -> Album -> Album
addATags orig last' = orig { albTracks = tracks
                          , albRelease = release
                          , albGenre = genre }
    where tracks  = P.zipWith addTTags (albTracks orig) (albTracks last')
          release = if isNothing (albRelease orig)
                        then albRelease last'
                        else albRelease orig
          genre   = if isNothing (albGenre orig)
                        then albGenre last'
                        else albGenre orig

addTTags :: Track -> Track -> Track
addTTags orig last' = orig { name = name', rank = rank' }
    where name' = if isNothing (name orig)
                      then name last'
                      else name orig
          rank' = if isNothing (rank orig)
                      then rank last'
                      else rank orig

query :: String -> String -> IO (Either String Album)
query art alb = do eresp <- try $ getAlbInfo (T.pack art) (T.pack alb)
                   case eresp of
                      Left err -> return $ Left (show (err :: SomeException))
                      Right mbalb -> case mbalb of
                                        Nothing -> return (Left "Failed to get Album Info")
                                        Just x -> return $ parseEither parseJSON x

getAlbInfo :: Text -> Text -> IO (Response JSON)
getAlbInfo art alb = lastfm $ LastAlbum.getInfo
                        <*> artist art
                        <*> album alb
                        <*> apiKey taggerapikey
                        <*  json

instance FromJSON Track where
    parseJSON (Object v) = do
        name' <- v .: "name"
        atts <- v .: "@attr"
        rank' <- atts .: "rank"
        return (Track "" "" (Just name')  (Just $ read rank'))
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
    where up [] = []
          up (x:xs) = C.toUpper x : xs
