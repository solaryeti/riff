module Main where

import           Test.Tasty
import           Test.Tasty.QuickCheck (testProperty)

import           Riff.SanitizeTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup "QuickCheck test"
    [ testGroup "Sanitize tests"
        [ testProperty "length is the same after removing invalid chars" prop_removeInvalid1
        , testProperty "only valid chars remain after transformation" prop_removeInvalid2
        ]
    ]
