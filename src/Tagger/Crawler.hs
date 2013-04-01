module Tagger.Crawler where

-- from base
import Control.Monad         (forM, filterM)

-- from filepath
import System.FilePath       ((</>), splitExtension)

-- from directory
import System.Directory      (doesDirectoryExist, getDirectoryContents)

import Tagger.Types

listArtists :: FilePath -> IO [Artist]
listArtists path = do
    artists <- getSubDirs path
    forM artists $ \name -> do
        albums <- listAlbums (path </> name)
        return (Artist name albums)

listAlbums :: FilePath -> IO [Album]
listAlbums path = do
    albums <- getSubDirs path
    forM albums $ \name -> do
        tracks <- listTracks (path </> name)
        return (Album name tracks Nothing Nothing)

listTracks :: FilePath -> IO [Track]
listTracks path = do
    tracks <- getDirectoryContents path
    let mp3s = filter isMp3 tracks
    return (map mkTrack mp3s)
  where mkTrack file = Track file path Nothing Nothing

isMp3 :: String -> Bool
isMp3 file | ext == ".mp3" = True
           | otherwise     = False
    where ext =  (snd . splitExtension) file

getSubDirs :: FilePath -> IO [FilePath]
getSubDirs path = getDirectoryContents path >>= filterM subdir
    where subdir x = if (x `elem` [".", ".."])
                     then return False
                     else doesDirectoryExist (path </> x)
