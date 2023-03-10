{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import           Test.Tasty
import           Test.Tasty.Hedgehog       (HedgehogTestLimit (..))

import qualified Spec.EmulatorSimpleType

main :: IO ()
main = defaultMain tests

-- | Number of successful tests for each hedgehog property.
--   The default is 100 but we use a smaller number here in order to speed up
--   the test suite.
--
limit :: HedgehogTestLimit
limit = HedgehogTestLimit (Just 5)

tests :: TestTree
tests = localOption limit $ testGroup " casos de uso " [
    Spec.EmulatorSimpleType.test
    ]
