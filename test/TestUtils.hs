module TestUtils ( getDataDir ) where

import System.Environment (getEnv)

getDataDir :: IO String
getDataDir = return "data"
