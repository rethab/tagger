{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative  ((<*>))
import           Control.Exception    (try, SomeException)
import           Data.Text            as T
import qualified Network.Lastfm.Album as LastAlbum
import           Network.Lastfm       (lastfm, (<*), artist, album, apiKey,
                                       json, Response, Format(JSON))
import           Data.Aeson.Types
import           System.Environment   (getArgs)

import qualified Network.HTTP.Conduit as C
import qualified Network.HTTP.Types   as C

main = do args <- getArgs
          res <- query (args !! 0) (args !! 1)
          case res of
            Left e -> putStrLn ("left: " ++ show e)
            Right r -> putStrLn ("right: " ++ show r)

query :: String -> String -> IO (Either SomeException String)
query art alb = do
        eresp <- try (loadWebsite "http://127.0.0.1:8080")
        --eresp <- try (getAlbInfo (T.pack art) (T.pack alb))
        case eresp of
          Left e -> return (Left e)
          Right resp -> return (Right "successfully parsed")

getAlbInfo :: Text -> Text -> IO (Response JSON)
getAlbInfo art alb = lastfm $ LastAlbum.getInfo
                        <*> artist art
                        <*> album alb
                        <*> apiKey "a2c21e95ab7239f87f2e5ff716fc6374"
                        <*  json

loadWebsite :: String -> IO String
loadWebsite rawUrl = do
        resp <- C.withManager $ \m ->
                   C.parseUrl rawUrl >>= flip C.httpLbs m >>= \t ->
                      return ( show (C.responseHeaders t))
        return (show resp)
