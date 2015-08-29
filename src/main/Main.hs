{- |
Module      :  <File name or $Header$ to be replaced automatically>
Description :  Rename files in a directory to only include a subset of chars
Copyright   :  (c) Steven Meunier
License     :  <BSD-style>

Maintainer  :  <steven@solaryeti.com>
Stability   :  experimental
Portability :  portable

Rename files in a directory so that they only include a certain characters
from a valid character set. Any invalid characters are replaced with an
underscore.
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module Main where

import Control.Exception as E (catch, try)
import Control.Monad          (filterM, liftM, when)
import Data.Char              (toLower)
import System.Exit
import System.IO.Error

import System.Console.CmdArgs (args, cmdArgs, def, help, typ, summary,
                               verbosity, whenLoud, (&=), Data, Typeable)

import Riff.Files
import Riff.Sanitize

type Directory = FilePath

data Options = Options
    { dryrun          :: Bool
    , lower           :: Bool
    , multiunderscore :: Bool
    , paths           :: [FilePath]
    , recurse         :: Bool
    , validchars      :: Bool
    } deriving (Show, Data, Typeable)

options :: Options
options = Options
    { dryrun = def &= help "Display changes without actually renaming anything"
    , lower = def &= help "Convert to lowercase"
    , multiunderscore = def &= help "Allow multiple underscores"
    , paths = def &= args &= typ "DIRS"
    , recurse = def &= help "Recurse into subdirectories"
    , validchars = def &= help "List the valid chars that filenames will consist of"
    } &=
    verbosity &=
    help "Sanitize filenames by replacing any chars not considered valid with _" &=
    summary "riff v0.0.0, (C) Steven Meunier 2015"

-- | Build a function that can be passed to 'transform' for transforming
-- filenames
buildTransformer :: Options -> Transformer
buildTransformer Options{..} = map
    (if lower then toLower else id) .
    removeUnderscoreBeforeDot .
    (if multiunderscore then id else removeDupUnderscore) .
    removeInvalid

main :: IO ()
main = do
    opts <- cmdArgs options
    when (validchars opts) $ putStrLn validChars >> exitSuccess
    when (dryrun opts) $ putStrLn "Executing dryrun. No files will be renamed"
    mapM_ (run opts) (paths opts)

-- | Main execution logic that is mapped to the paths provided by the user.
-- A listing of directories in the path are fetched. If there are any
-- subdirectories then these are processed first before renaming files
-- in the given path, followed by directories.
run :: Options -> Directory -> IO ()
run opts dir = do
    when (recurse opts) $ do
      ds <- E.try $ dirs dir :: IO (Either IOError [FilePath])
      case ds of
        Left e -> handleIOError e >> exitFailure
        Right xs -> mapM_ (run opts) xs
    dirContents dir >>= \ys ->
      renamefunc (filePairs transformer ys) `E.catch` handleIOError
  where
    transformer = buildTransformer opts
    renamefunc = if dryrun opts then doDryrun else doRename

    handleIOError :: IOError -> IO ()
    handleIOError e
      | isDoesNotExistError e = case ioeGetFileName e of
          Nothing -> putStrLn "File does not exist"
          Just s  -> putStrLn $ s ++ ": file or directory does not exist"
      | otherwise = putStrLn $ "You made a huge mistake but I don't know what it is!\n" ++
                    "But I'll give you a hint: " ++ ioeGetErrorString e

doRename :: FilePairs -> IO ()
doRename xs = do
    -- Inform about files that cannot be renamed because a file with
    -- the new name already exists
    whenLoud (filterM newExist xs >>= mapM_ (putStrLn . informExists))
    whenLoud (renameableFilePairs >>= mapM_ print )
    (renameableFilePairs >>= mapM_ rename) `E.catch` handler
  where
    renameableFilePairs = filterM (liftM not . newExist) xs
    informExists x = "Skipping " ++ show x ++ " already exists."

    handler :: IOError -> IO ()
    handler e = putStrLn $ "Skipping " ++ show e

doDryrun :: FilePairs -> IO ()
doDryrun = mapM_ print
