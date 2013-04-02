module TestUtils ( getDataDir ) where

import Control.Monad
import System.Environment (getEnv)
import Test.QuickCheck
import Tagger.Types

getDataDir :: IO String
getDataDir = return "data"

instance Arbitrary Track where
    arbitrary = liftM4 Track arbitrary arbitrary arbitrary arbitrary


