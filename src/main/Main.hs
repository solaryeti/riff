{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module Main where

import Control.Exception as E (catch, try)
import Control.Monad (filterM, liftM, when)
import Data.Char (toLower)
import System.Console.CmdArgs
import System.Directory (doesFileExist, doesDirectoryExist, renameDirectory, renameFile)
import System.Exit
import System.FilePath (combine, splitFileName, takeFileName)
import System.IO.Error

import Riff.Files
import Riff.Sanitize

data FileType = Dirs | Files
type Directory = FilePath
type Transformer = String -> String

data Options = Options
    { dryrun  :: Bool
    , lower   :: Bool
    , multiunderscore :: Bool
    , paths   :: [FilePath]
    , recurse :: Bool
    , validchars :: Bool
    } deriving (Show, Data, Typeable)

options :: Options
options = Options
    {
      dryrun = def &= help "Display changes without actually renaming anything"
    , lower = def &= help "Convert to lowercase"
    , multiunderscore = def &= help "Allow multiple underscores"
    , paths = def &= args &= typ "FILES/DIRS"
    , recurse = True &= help "Recurse into subdirectories"
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

-- | Transform a 'FilePath' using the given 'Transformer'
transform :: Transformer -> FilePath -> FilePath
transform f = uncurry combine . mapSnd f . splitFileName

-- | Apply a function to the first element of a pair
mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (x, y) = (f x, y)

-- | Apply a function to the second element of a pair
mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)

main :: IO ()
main = do
  opts <- cmdArgs options
  when (validchars opts) $ putStrLn validChars >> exitSuccess
  when (dryrun opts) $ putStrLn "Executing dryrun. No files will be renamed"
  mapM_ (run opts) (paths opts)

run :: Options -> Directory -> IO ()
run opts p = do
    ds <- E.try $ dirs p :: IO (Either IOError [FilePath])
    case ds of
      Left e -> handleIOError e >> exitFailure

      -- rename files if there are no more dirs to descend into
      Right [] -> renamefunc Files transformer p

      -- received a listing containing subdirectories
      Right xs -> do
          mapM_ (run opts) xs
          renamefunc Files transformer p

          -- Rename directories only after we have descended into them
          renamefunc Dirs transformer p

  where transformer = buildTransformer opts
        renamefunc = if dryrun opts then doDryrun else rename

        handleIOError :: IOError -> IO ()
        handleIOError e
          | isDoesNotExistError e = case ioeGetFileName e of
              Nothing -> putStrLn "File does not exist"
              Just s  -> putStrLn $ s ++ ": file or directory does not exist"
          | otherwise = putStrLn $ "You made a huge mistake but I don't know what it is!\n" ++
                        "But I'll give you a hint: " ++ ioeGetErrorString e


newNames :: FileType -> Transformer -> Directory -> IO [(FilePath, FilePath)]
newNames t f p = filter (uncurry (/=)) <$> map (\x -> (x, transform f x)) <$> g p
  where g = case t of
          Dirs  -> dirs
          Files -> files

rename :: FileType -> Transformer -> Directory -> IO ()
rename t f x = do
    filenamePairs <- newNames t f x
    pairs <- filterM (liftM not . newExists) filenamePairs

    whenLoud (mapM_ (putStrLn . inform) pairs)

    -- Inform about files that cannot be renamed because a file with
    -- the new name already exists
    whenLoud (filterM newExists filenamePairs >>= mapM_ (putStrLn . informExists))

    mapM_ doRename pairs `E.catch` handler

  where inform (from, to) = from ++ " -> " ++ takeFileName to
        informExists (from, to) = "Skipping file: " ++ from ++ ": " ++
                                  takeFileName to ++ " already exists."
        newExists (_, to) = existsfunc t to
        existsfunc Files = doesFileExist
        existsfunc Dirs = doesDirectoryExist
        doRename (from, to) = renamefunc t from to
        renamefunc Files = renameFile
        renamefunc Dirs = renameDirectory
        handler :: IOError -> IO ()
        handler e = putStrLn $ "Skipping file: " ++ show e

doDryrun :: FileType -> Transformer -> Directory -> IO ()
doDryrun t f x = newNames t f x >>= mapM_ (putStrLn . inform)
  where inform (from, to) = from ++ " -> " ++ takeFileName to
