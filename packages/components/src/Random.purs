module KSF.Random where

import Prelude

import Data.Char.Gen (genAlpha)
import Data.String.CodeUnits (fromCharArray)
import Effect (Effect)
import Test.QuickCheck.Gen (randomSample')

randomString :: Int -> Effect String
randomString len = do
  fromCharArray <$> randomSample' len genAlpha
