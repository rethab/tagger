module Tagger.TagWriter where

-- from base
import           Control.Applicative       ((<$>))
import           Data.Maybe                (fromJust)

-- from filepath
import           System.FilePath           ((</>))

-- from taglib
import qualified Sound.TagLib as T

-- from transformers
import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)


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
writeTag tag mbrel mbgen trk = do
    mbwrite (toInteger <$> mbrel) T.setYear
    mbwrite mbgen T.setGenre
    mbwrite (name trk) T.setTitle
    mbwrite (toInteger <$> rank trk) T.setTrack
 where mbwrite :: Maybe a -> (T.Tag -> a -> IO ()) -> IO ()
       mbwrite Nothing _  = return ()
       mbwrite (Just x) f = f tag x
