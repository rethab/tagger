module TestUtils ( getDataDir ) where

import Control.Monad      (liftM2, liftM4)
import System.Environment (getEnv)
import Test.QuickCheck
import Tagger.Types

getDataDir :: IO String
getDataDir = return "data"

instance Arbitrary Artist where
    arbitrary = liftM2 Artist arbitrary arbitrary

instance Arbitrary Album where
    arbitrary = liftM4 Album arbitrary arbitrary arbitrary arbitrary

instance Arbitrary Track where
    arbitrary = liftM4 Track arbitrary arbitrary arbitrary arbitrary
