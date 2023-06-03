{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : Riff.Options
Description : Commandline Options
Copyright   : (c) 2023 Steven Meunier
License     : BSD-style (see the file LICENSE)
-}
module Riff.Options
  ( Options (..)
  , getOpts
  ) where

import Riff.Prelude

import Data.Version (showVersion)
import Development.GitRev
  ( gitCommitDate
  , gitHash
  )
import Options.Applicative
import Paths_riff (version) -- Magic module that gets the version from the cabal file

data Options = Options
  { apostrophe :: Bool
  , dryrun :: Bool
  , lower :: Bool
  , multiunderscore :: Bool
  , hyphen :: Bool
  , recurse :: Bool
  , validchars :: Bool
  , verbose :: Bool
  , paths :: [FilePath]
  }
  deriving (Show)

getOpts :: IO Options
getOpts = execParser optsParser

optsParser :: ParserInfo Options
optsParser =
  info
    (customHelper <*> versionOption <*> parseOpts)
    ( fullDesc
        <> progDesc "Sanitize filenames by replacing any chars not considered valid with _"
        <> header "riff - filename sanitizer"
    )
 where
  customHelper =
    abortOption
      (ShowHelpText Nothing)
      (mconcat [long "help", help "Show this help text", hidden])

parseOpts :: Parser Options
parseOpts = do
  apostrophe <-
    switch $
      mconcat
        [ long "apostrophe"
        , short 'a'
        , help "Drop apostrophes instead of replacing them with an underscore."
        ]

  dryrun <-
    switch $
      mconcat
        [ long "dryrun"
        , short 'n'
        , help "Display changes without actually renaming anything. Implies verbose output."
        ]

  lower <-
    switch $
      mconcat
        [ long "lower"
        , short 'l'
        , help "Convert to lowercase"
        ]

  multiunderscore <-
    switch $
      mconcat
        [ long "multiunderscore"
        , short 'm'
        , help "Allow multiple underscores"
        ]

  hyphen <-
    switch $
      mconcat
        [ long "hyphen"
        , short 'h'
        , help "Neaten hyphens by removing underscores to the left and right of them"
        ]

  recurse <-
    switch $
      mconcat
        [ long "recurse"
        , short 'r'
        , help "Recurse into subdirectories"
        ]

  validchars <-
    switch $
      mconcat
        [ long "validchars"
        , help "List the valid chars that filenames will consist of"
        ]

  verbose <-
    switch $
      mconcat
        [ long "verbose"
        , short 'v'
        , help "Enable verbose output."
        ]

  paths <- many $ strArgument (metavar "FILES/DIRS")

  pure Options{..}

-- | Version option that gets the version from the cabal file and the
-- git hash.
versionOption :: Parser (a -> a)
versionOption =
  infoOption
    ( concat
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
