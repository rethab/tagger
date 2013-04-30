module Main where

-- from base
import Data.Either               (partitionEithers)
import Control.Applicative       ((<$>))
import Control.Monad             (when)
import System.Environment        (getArgs)

-- from directory
import System.Directory          (getCurrentDirectory)

import Tagger.Crawler            (listArtists)
import Tagger.IncompletionFinder (getIncomplete)
import Tagger.TagCompleter       (complete)
import Tagger.TagWriter          (writeTags)
import Tagger.Types

main :: IO ()
main = do args <- getArgs
          dir <- if null args then getCurrentDirectory else return (head args)
          main' dir

main' dir = do putStrLn "Tagger 0.1, (c) rethab 2013"
               putStr ("Search Artists in " ++ dir ++ ".. ")
               artists <- listArtists dir
               putStrLn ("found " ++ show (length artists) ++ " Artists.")
               putStr "Filter incomplete.. "
               incomplete <- getIncomplete artists 
               putStrLn (show (length incomplete) ++ " are incomplete.")
               putStr ("Complete ID3 Tags.. ")
               (errs, completed) <- partitionEithers <$> complete incomplete
               when (not . null $ errs) (putStrLn "errors:" >> mapM_ putStrLn errs)
               putStrLn $ show (length completed) ++ " were successful"
               when (not . null $ completed) $ do
                   putStrLn "Write back? "
                   wb <- getLine
                   if (wb `elem` ["yes", "y"])
                       then do putStr "Write back.. "
                               writeTags completed
                               putStrLn "done."
                       else do putStrLn "No writing back: "
                               mapM_ (putStrLn . show) completed


artist = Artist "Inexistent" [album]
album = Album "Inexistent" [track] Nothing Nothing
track = Track "" "" Nothing Nothing
runmain = do (errs, completed) <- partitionEithers <$> complete [artist]
             mapM_ putStrLn errs
