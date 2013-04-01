module Tagger.IncompletionFinder where

-- from base
import           Data.Maybe                (isNothing)
import           Control.Applicative       ((<$>), (<*>))
import           Control.Monad             (mapM, liftM2)

-- from filepath
import           System.FilePath           ((</>))

-- from taglib
import qualified Sound.TagLib as T

-- from mtl
import           Control.Monad.Trans       (lift)

-- from transformers
import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

import           Tagger.Types
import           Tagger.Crawler            (listArtists)


-- | Read ID3 Tags and set them on the Songs and Albums of an Artist,
-- | filter those with incomplete Tags.
getIncomplete :: [Artist] -> IO [Artist]
getIncomplete as = mapM tagArt as >>= return . filter incomp

-- | If any Tag is not set
incomp :: Artist -> Bool
incomp (Artist artName artAlbs) = or (map incompAlb artAlbs)
    where incompAlb alb = or [ isNothing (albRelease alb)
                             , isNothing (albGenre alb)
                             , or (map incompTrk (albTracks alb))]
          incompTrk trk = or [ isNothing (name trk)
                             , isNothing (rank trk)]

-- | Read Tags for each Album
tagArt :: Artist -> IO Artist
tagArt art = do albs <- mapM tagAlb (artAlbs art)
                return $ art { artAlbs = albs }

-- | Read Tags for each Track and set Album Info from Tracks
tagAlb :: Album -> IO Album
tagAlb alb = do albTrkTags <- mapM tagTrk (albTracks alb)
                return $ alb { albTracks = map fst albTrkTags
                             , albRelease = fst . head $ map snd albTrkTags
                             , albGenre = snd . head $ map snd albTrkTags }
                -- TODO replace head with sensible thing

-- | Read Tag for one Track as well as ReleaseDate and Genre
tagTrk :: Track -> IO (Track, (Maybe Int, Maybe String))
tagTrk trk = do mbTrk <- runMaybeT tagTrk'
                case mbTrk of
                   Nothing -> return (trk, (Nothing, Nothing))
                   Just x -> return x
  where path    = location trk </> file trk
        tagTrk' = do tagFile <- MaybeT (T.open path)
                     tag <- MaybeT (T.tag tagFile)
                     liftM2 (,) (lift $ readTTags trk tag)
                                (lift $ readATags tag)
    
-- | Convert Tags into Song. Interpret empty strings and zeroes
-- | as missing information
readTTags :: Track -> T.Tag -> IO Track
readTTags track tag = do
    name' <- smb <$> T.title tag
    rank' <- (imb . fromInteger) <$> T.track tag
    return $ track { name = name', rank = rank' }

-- | Read Album ID3 Tags
readATags :: T.Tag -> IO (Maybe Int, Maybe String)
readATags tag = (,) <$> ((imb . fromInteger) <$> T.year tag)
                    <*> (smb <$> T.genre tag)

-- | Interpret empty String as Nothing
smb :: String -> Maybe String
smb "" = Nothing
smb x  = Just x

-- | Interpret zero as Nothing
imb :: Int -> Maybe Int
imb 0  = Nothing
imb x  = Just x
