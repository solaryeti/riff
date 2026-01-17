{- |
Module      : Riff.Prelude
Description : Custom Prelude
Copyright   : (c) 2026 Steven Meunier
License     : BSD-style (see the file LICENSE)

Welcome to your custom Prelude
Export here everything that should always be in your library scope
For more info on what is exported by Protolude check:
https://github.com/sdiehl/protolude/blob/master/Symbols.md
-}
module Riff.Prelude
  ( module Exports
  , id
  , String
  ) where

import Data.Function (id)
import Protolude as Exports

type String = [Char]
