module Tagger.TagWriter where

import           Control.Applicative       ((<$>))
import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import           Data.Maybe                (fromJust)
import qualified Sound.TagLib as T
import           System.FilePath           ((</>))
import           Tagger.Types

writeTags :: [Artist] -> IO ()
writeTags = mapM_ (mapM_ writeAlb . artAlbs)

writeAlb :: Album -> IO ()
writeAlb alb = mapM_ (writeTrack (albRelease alb) (albGenre alb)) (albTracks alb)

writeTrack :: Maybe Int -> Maybe String -> Track -> IO ()
writeTrack rel gen trk = do tagfile <- fromJust <$> T.open path
                            tag <- fromJust <$> T.tag tagfile
                            writeTag tag rel gen trk
                            T.save tagfile
                            return ()
    where path = location trk </> file trk

writeTag :: T.Tag -> Maybe Int -> Maybe String -> Track -> IO ()
writeTag tag mbrel mbgen trk = do case mbrel of
                                    Nothing -> return ()
                                    Just rel -> T.setYear tag (toInteger rel)
                                  case mbgen of
                                    Nothing -> return ()
                                    Just gen -> T.setGenre tag gen
                                  case name trk of
                                    Nothing -> return ()
                                    Just title -> T.setTitle tag title
                                  case rank trk of
                                    Nothing -> return ()
                                    Just track -> T.setTrack tag (toInteger track)
