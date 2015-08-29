-- | String sanitation

module Riff.Sanitize
    (
      -- * Transformer Functions
      removeInvalid
    , removeDupUnderscore
    , removeUnderscoreBeforeDot
      -- * Valid Characters
    , validChars
    ) where

import Data.Char (intToDigit)
import Data.List (group)

-- | Replace any characters in the string that are not part of 'validChars'
-- with an underscore.
removeInvalid :: String -> String
removeInvalid = foldr invalidToUnderscore ""
    where invalidToUnderscore x ys
            | x `elem` validChars = x : ys
            | otherwise = '_' : ys

-- | Remove duplicate underscores from a string.
removeDupUnderscore :: String -> String
removeDupUnderscore = concatMap squash . group
  where squash ('_':_) = "_"
        squash xs = xs

-- | Remove underscores before an extension so '_.' becomes '.'
removeUnderscoreBeforeDot :: String -> String
removeUnderscoreBeforeDot [] = []
removeUnderscoreBeforeDot [x] = [x]
removeUnderscoreBeforeDot ('_':'.':xs) = '.' : removeUnderscoreBeforeDot xs
removeUnderscoreBeforeDot (x:xs) = x : removeUnderscoreBeforeDot xs

-- | A list of valid characters for file names.
validChars :: String
validChars = concat [ ['a'..'z']
                    , ['A'..'Z']
                    , map intToDigit [0..9]
                    , "-_." ]
