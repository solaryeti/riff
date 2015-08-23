-- | String sanitation

module Riff.Sanitize
       ( removeInvalid
       , removeDupUnderscore
       , validChars
       ) where

import Data.Char (intToDigit)
import Data.List (group)

-- | A list of valid characters for file names.
validChars :: String
validChars = concat [ ['a'..'z']
                    , ['A'..'Z']
                    , map intToDigit [0..9]
                    , "_." ]

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

-- removeDupExtension :: String -> String
-- removeDupExtension s = f (groupBy (\_ x -> x /= '.') s)
--   where f [[]] = ""
--         f [[x]] = [x]
--         f [(x:xs)] = case x of
--           'a' -> "aaaa"
--           _ -> "bbbb"
--         f [xs] = xs

-- foo = case "abc" of
--   'a':xs -> "aaaaa"
--   _ -> "bbbb"
