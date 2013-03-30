module Tagger.Types
    ( Artist (..)
    , Album (..)
    , Track (..)
    ) where

data Artist = Artist
    { artName :: String
    , artAlbs :: [Album]
    } deriving (Show, Eq)

data Album = Album
    { albName :: String
    , albTracks :: [Track]
    , albRelease :: Maybe Int
    , albGenre :: Maybe String
    } deriving (Show, Eq)

data Track = Track
    { file :: FilePath
    , location :: FilePath
    , name :: Maybe String
    , rank :: Maybe Int
    } deriving (Show, Eq)
