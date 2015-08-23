module Riff.SanitizeTest where

-- import Test.QuickCheck
import Riff.Sanitize

prop_removeInvalid1 :: String -> Bool
prop_removeInvalid1 s = length s == length (removeInvalid s)

prop_removeInvalid2 :: String -> Bool
prop_removeInvalid2 s = all (`elem` validChars) (removeInvalid s)
