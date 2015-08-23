module Main where

import Test.Tasty
-- import Test.Framework.Providers.HUnit
import Test.Tasty.QuickCheck (testProperty)

-- import Test.QuickCheck
-- import Test.HUnit

import Riff.SanitizeTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup "QuickCheck test"
    [ testGroup "Sanitize tests"
        [ testProperty "removeInvalid1" prop_removeInvalid1
        , testProperty "removeInvalid2" prop_removeInvalid2
        ]
    ]
