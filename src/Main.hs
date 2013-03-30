module Main where

import System.Directory          (getCurrentDirectory)
import System.Environment        (getArgs)
import Tagger.Crawler            (listArtists)
import Tagger.IncompletionFinder (getIncomplete)
import Tagger.TagCompleter       (complete, completePar)
import Tagger.TagWriter          (writeTags)

main :: IO ()
main = do args <- getArgs
          dir <- if null args then getCurrentDirectory else return (head args)
          putStrLn "Tagger 0.1, (c) rethab 2013"
          putStr ("Search Artists in " ++ dir ++ ".. ")
          artists <- listArtists dir
          putStrLn ("found " ++ show (length artists) ++ " Artists.")
          putStr "Filter incomplete.. "
          incomplete <- getIncomplete artists 
          putStrLn (show (length incomplete) ++ " are incomplete.")
          putStr ("Complete ID3 Tags.. ")
          completed <- completePar incomplete
          putStrLn "done."
          putStrLn "Write back? "
          wb <- getLine
          if (wb `elem` ["yes", "y"])
              then do putStr "Write back.. "
                      writeTags completed
                      putStrLn "done."
              else do putStrLn "No writing back: "
                      mapM_ (putStrLn . show) completed
