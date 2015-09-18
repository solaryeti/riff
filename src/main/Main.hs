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

import           Riff.Files
import           Riff.Sanitize

import           Control.Exception as E (catch, try)
import           Control.Monad (filterM, when, unless)
import           Data.Char (toLower)
import qualified Data.Set as Set (toList)

import           System.Exit
import           System.IO.Error
import           System.Posix.Files (isRegularFile, getFileStatus)

import System.Console.CmdArgs (args, cmdArgs, def, help, typ,
                               setVerbosity, summary, verbosity,
                               whenLoud, (&=),
                               Data, Typeable, Verbosity(..))

data Options = Options
    { apostrophe      :: Bool
    , dryrun          :: Bool
    , lower           :: Bool
    , multiunderscore :: Bool
    , paths           :: [FilePath]
    , recurse         :: Bool
    , validchars      :: Bool
    } deriving (Show, Data, Typeable)

options :: Options
options = Options
    { apostrophe = def &= help "Drop apostrophes instead of replacing them with an underscore."
    , dryrun = def &= help "Display changes without actually renaming anything. Implies verbosity."
    , lower = def &= help "Convert to lowercase"
    , multiunderscore = def &= help "Allow multiple underscores"
    , paths = def &= args &= typ "FILES/DIRS"
    , recurse = def &= help "Recurse into subdirectories"
    , validchars = def &= help "List the valid chars that filenames will consist of"
    } &=
    verbosity &=
    help "Sanitize filenames by replacing any chars not considered valid with _" &=
    summary "riff v0.1.0, (C) Steven Meunier 2015"

-- | Build a function that can be passed to 'transform' for transforming
-- filenames
buildTransformer :: Options -> Transformer
buildTransformer Options{..} = map
    (if lower then toLower else id)
    . removeUnderscoreBeforeDot
    . (if multiunderscore then id else removeDupUnderscore)
    . removeInvalid
    . (if apostrophe then dropApostrophe else id)

main :: IO ()
main = do
    opts <- cmdArgs options
    when (validchars opts) $ putStrLn (Set.toList validChars) >> exitSuccess
    when (dryrun opts) $ setVerbosity Loud >> putStrLn "Executing dryrun. No files will be renamed"
    mapM_ (run opts) (paths opts)

-- | Main execution logic that is mapped to the paths provided by the user.
-- If the user has opted to recurse, then any subdirectories are
-- processed first before renaming files and directories in the given
-- path.
run :: Options -> FilePath -> IO ()
run opts path = do
    rf <- E.try (isRegularFile <$> getFileStatus path) :: IO (Either IOError Bool)
    case rf of
        Left e -> handleIOError e >> exitFailure
        Right True -> doRename [path]
        Right False -> do
          rd <- E.try $ dirContents path :: IO (Either IOError [FilePath])
          case rd of
              Left e -> handleIOError e >> exitFailure
              Right xs -> do
                  when (recurse opts) $ dirs path >>= mapM_ (run opts)
                  doRename xs
  where
    doRename ys = do
        (whenLoud . inform . getFilePairs) ys
        unless (dryrun opts) $ E.catch (renameableFilePairs (getFilePairs ys) >>= mapM_ rename) handleIOError
    getFilePairs = filePairs $ buildTransformer opts

inform :: FilePairs -> IO ()
inform xs = do
    filterM newExist xs >>= mapM_ (putStrLn . informExists)
    renameableFilePairs xs >>= mapM_ print
  where
    informExists x = "Skipping " ++ show x ++ " already exists."

handleIOError :: IOError -> IO ()
handleIOError e
  | isPermissionError e = putStrLn $ "Skipping " ++ show e
  | isDoesNotExistError e = case ioeGetFileName e of
      Nothing -> putStrLn "File does not exist"
      Just s  -> putStrLn $ "Aborting: " ++ s ++ ": file or directory does not exist"
  | otherwise = putStrLn $ "You made a huge mistake but I don't know what it is!\n" ++
                "But I'll give you a hint: " ++ ioeGetErrorString e
