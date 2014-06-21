module Main where

import Control.Exception  (try, SomeException)
import System.Environment (getArgs)

import Network.HTTP.Conduit as C
import Network.HTTP.Types   as C

main = do url <- (!! 0) `fmap` getArgs
          eresp <- try (loadWebsite url)
          case eresp of
            Left e -> putStrLn ("Falsch: " ++ show (e :: SomeException))
            Right r -> putStrLn ("All Cool: " ++ r)

loadWebsite :: String -> IO String
loadWebsite rawUrl = do
        resp <- C.withManager $ \m ->
                   C.parseUrl rawUrl >>= flip C.httpLbs m >>= \t ->
                      return ( show (C.responseHeaders t))
        return (show resp)
