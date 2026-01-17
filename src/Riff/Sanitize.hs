{- |
Module      : Riff.Sanitize
Description : Functions for string sanitation
Copyright   : (c) 2026 Steven Meunier
License     : BSD-style (see the file LICENSE)
-}
module Riff.Sanitize
  ( -- * Transformer Functions
    transform
  , dropApostrophe
  , neatenHyphen
  , removeInvalid
  , removeDupUnderscore
  , removeUnderscoreBeforeDot

    -- * Valid Characters
  , validChars

    -- * Types
  , Transformer
  )
where

import qualified Data.Set as Set
  ( Set
  , fromList
  , member
  )
import Riff.Prelude
import System.FilePath
  ( combine
  , splitFileName
  )

-- | A function that performs a transformation on a string. The
-- transformation is carried out by the 'transform' function.
type Transformer = String -> String

-- | Transform a 'FilePath' using the given 'Transformer'
transform :: Transformer -> FilePath -> FilePath
transform transformer = uncurry combine . mapSnd transformer . splitFileName
 where
  mapSnd f (x, y) = (x, f y)

-- | Remove apostrophes from a 'String'.
dropApostrophe :: String -> String
dropApostrophe = filter ('\'' /=)

-- | Remove underscores on the left or right side of a hyphen.
neatenHyphen :: String -> String
neatenHyphen = foldr helper ""
 where
  helper '_' ('-' : ys) = '-' : ys
  helper '-' ('_' : ys) = '-' : ys
  helper x ys = x : ys

-- | Replace any characters in the string that are not part of 'validChars'
-- with an underscore.
removeInvalid :: String -> String
removeInvalid = foldr invalidToUnderscore ""
 where
  invalidToUnderscore x ys
    | x `Set.member` validChars = x : ys
    | otherwise = '_' : ys

-- | Remove duplicate underscores from a string.
removeDupUnderscore :: String -> String
removeDupUnderscore = concatMap squash . group
 where
  squash ('_' : _) = "_"
  squash xs = xs

-- | Remove underscores before an extension so '_.' becomes '.'
removeUnderscoreBeforeDot :: String -> String
removeUnderscoreBeforeDot = foldr helper ""
 where
  helper '_' ('.' : ys) = '.' : ys
  helper x ys = x : ys

-- | A list of valid characters for file names.
validChars :: Set.Set Char
validChars =
  Set.fromList $
    concat [['a' .. 'z'], ['A' .. 'Z'], map intToDigit [0 .. 9], "-_."]
