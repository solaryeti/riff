{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module Main where

import Control.Exception as E (try)
import Data.Char (toLower)
-- import GHC.IO.Exception
import System.Console.CmdArgs
import System.FilePath (combine, splitFileName, takeFileName)
import System.IO.Error
import System.Exit

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

    } deriving (Show, Data, Typeable)

options :: Options
options = Options
    {
      dryrun = True &= help "Display changes without actually renaming anything"
    , lower = def &= help "Convert to lowercase"
    , multiunderscore = def &= help "Allow multiple underscores"
    , paths = def &= args &= typ "FILES/DIRS"
    , recurse = True &= help "Recurse into subdirectories"
    } &=
    verbosity &=
    help "Sanitize filenames by replacing any non-alphanumeric chars with _" &=
    summary "riff v0.0.0, (C) Steven Meunier 2015"

-- | Build a function that can be passed to 'transform' for transforming
-- filenames
buildTransformer :: Options -> Transformer
buildTransformer Options{..} = map
    (if lower then toLower else id) .
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
main = cmdArgs options >>= \opts ->
  mapM_ (run opts) (paths opts)
-- main = print =<< cmdArgs options

newNames :: FileType -> Transformer -> Directory -> IO [(FilePath, FilePath)]
newNames t f p = filter (uncurry (/=)) <$> map (\x -> (x, transform f x)) <$> g p
  where g = case t of
          Dirs  -> dirs
          Files -> files

rename :: FileType -> Transformer -> Directory -> IO ()
-- rename t x = (map snd <$> newNames t x) >>= mapM_ putStrLn
rename t f x = do
    filenamePairs <- newNames t f x
    whenLoud (mapM_ (putStrLn . inform) filenamePairs)
  where inform (from, to) = from ++ " -> " ++ takeFileName to

run :: Options -> Directory -> IO ()
run opts p = do
    ds <- E.try $ dirs p :: IO (Either IOError [FilePath])
    case ds of
      Left e -> handleIOError e >> exitFailure

      -- rename files if there are no more dirs to descend into
      Right [] -> rename Files transformer p

      -- received a listing containing subdirectories
      Right xs -> do
          mapM_ (run opts) xs
          rename Files transformer p

          -- Rename directories only after we have descended into them
          rename Dirs transformer p

  where transformer = buildTransformer opts

handleIOError :: IOError -> IO ()
handleIOError e
  | isDoesNotExistError e = case ioeGetFileName e of
                              Nothing -> putStrLn "File does not exist"
                              Just s  -> putStrLn $ s ++ ": file or directory does not exist"
  | otherwise = putStrLn $ "You made a huge mistake but I don't know what it is!\n" ++
                           "But I'll give you a hint: " ++ ioeGetErrorString e
