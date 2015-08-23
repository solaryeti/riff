{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module Main where

import Data.Char (toLower)
import System.Console.CmdArgs
import System.FilePath (combine, splitFileName, takeFileName)

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
    ds <- dirs p
    case ds of
      [] -> rename Files transformer p -- rename files if there are no more dirs to descend into
      xs -> do
        mapM_ (run opts) xs
        rename Files transformer p

        -- Rename directories only after we have descended into them
        rename Dirs transformer p
  where transformer = buildTransformer opts



-- Let's handle the IO exceptions in main
-- import Control.Exception (try)
-- import System.IO.Error
--   do
--   dirContents <- try (getDirectoryContents x)  :: IO (Either IOError [FilePath])
--   case dirContents of
--     Left _ -> Nothing
--     Right fs -> Just $ absPaths (filterSpecial' fs)
--       where absPaths = mapM (canonicalizePath . (x </>))

-- filterSpecial' :: [FilePath] -> [FilePath]
-- filterSpecial' = filter (\x -> x /= "." && x /= "..")
