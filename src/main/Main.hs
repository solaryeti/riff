{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module Main where

import Data.Char (toLower)
import System.Console.CmdArgs
import System.FilePath (combine, splitFileName, takeFileName)

import Riff.Files
import Riff.Sanitize

data FileType = Dirs | Files
type Directory = FilePath

data Options = Options
    { recurse :: Bool
    , lower   :: Bool
    , dryrun  :: Bool
    , paths   :: [FilePath]
    } deriving (Show, Data, Typeable)

options :: Options
options = Options
    { recurse = True &= help "Recurse into subdirectories"
    , lower = def &= help "Convert to lowercase"
    , dryrun = True &= help "Display changes without actually renaming anything"
    , paths = def &= args &= typ "FILES/DIRS"
    } &=
    verbosity &=
    help "Sanitize filenames by replacing any non-alphanumeric chars with _" &=
    summary "riff v0.0.0, (C) Steven Meunier 2015"

transform :: String -> String
transform = uncurry combine . mapSnd transformers . splitFileName
  where transformers = map toLower . removeDupUnderscore . removeInvalid

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (x, y) = (f x, y)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)

main :: IO ()
main = mapM_ run . paths =<< cmdArgs options
-- main = print =<< cmdArgs options

newNames :: FileType -> Directory -> IO [(FilePath, FilePath)]
newNames t p = filter (uncurry (/=)) <$> map (\x -> (x, transform x)) <$> f p
  where f = case t of
          Dirs  -> dirs
          Files -> files

rename :: FileType -> Directory -> IO ()
-- rename t x = (map snd <$> newNames t x) >>= mapM_ putStrLn
rename t x = do
    filenamePairs <- newNames t x
    whenLoud (mapM_ (putStrLn . inform) filenamePairs)
  where inform (from, to) = from ++ " -> " ++ takeFileName to

run :: Directory -> IO ()
run p = do
    ds <- dirs p
    case ds of
      [] -> rename Files p -- rename files if there are no more dirs to descend into
      xs -> do
        mapM_ run xs
        rename Files p

        -- Rename directories only after we have descended into them
        rename Dirs p

-- Recurses to one level deep. What about arbitrary levels?
-- recurse :: Directory -> IO [[(FilePath, FilePath)]]
-- recurse p = do
--    ds <- newDirNames p
--    let d = map fst ds
--    mapM newFileNames d
