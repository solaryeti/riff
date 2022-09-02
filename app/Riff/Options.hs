{-|
Module      : Riff.Options
Description : Commandline Options
Copyright   : (c) 2022 Steven Meunier
License     : BSD-style (see the file LICENSE)
-}

{-# LANGUAGE TemplateHaskell #-}

module Riff.Options
  ( Options(..)
  , getOpts
  ) where

import           Riff.Prelude

import           Data.Version                   ( showVersion )
import           Development.GitRev             ( gitCommitDate
                                                , gitHash
                                                )
import           Options.Applicative
import           Paths_riff                     ( version ) -- Magic module that gets the version from the cabal file

data Options = Options
  { apostrophe      :: Bool
  , dryrun          :: Bool
  , lower           :: Bool
  , multiunderscore :: Bool
  , hyphen          :: Bool
  , recurse         :: Bool
  , validchars      :: Bool
  , verbose         :: Bool
  , paths           :: [FilePath]
  }
  deriving Show

getOpts :: IO Options
getOpts = execParser optsParser

optsParser :: ParserInfo Options
optsParser = info
  (customHelper <*> versionOption <*> parseOpts)
  (  fullDesc
  <> progDesc
       "Sanitize filenames by replacing any chars not considered valid with _"
  <> header "riff - filename sanitizer"
  )
 where
  customHelper = abortOption
    (ShowHelpText Nothing)
    (mconcat [long "help", help "Show this help text", hidden])

parseOpts :: Parser Options
parseOpts =
  Options
    <$> switch
          (long "apostrophe" <> short 'a' <> help
            "Drop apostrophes instead of replacing them with an underscore."
          )
    <*> switch
          (  long "dryrun"
          <> short 'n'
          <> help
               "Display changes without actually renaming anything. Implies verbose output."
          )
    <*> switch (long "lower" <> short 'l' <> help "Convert to lowercase")
    <*> switch
          (long "multiunderscore" <> short 'm' <> help
            "Allow multiple underscores"
          )
    <*> switch
          (  long "hyphen"
          <> short 'h'
          <> help
               "Neaten hyphens by removing underscores to the left and right of them"
          )
    <*> switch
          (long "recurse" <> short 'r' <> help "Recurse into subdirectories")
    <*> switch
          (  long "validchars"
          <> help "List the valid chars that filenames will consist of"
          )
    <*> switch (long "verbose" <> short 'v' <> help "Enable verbose output.")
    <*> many (strArgument (metavar "FILES/DIRS"))

-- | Version option that gets the version from the cabal file and the
-- git hash.
versionOption :: Parser (a -> a)
versionOption = infoOption
  (concat
    [ "riff "
    , showVersion version
    , ", Git revision "
    , $(gitHash)
    , " ("
    , $(gitCommitDate)
    , ")"
    ]
  ) -- TemplateHaskell required for gitrev functions.
  (long "version" <> help "Show version")
