-- | Directory listing functions

module Riff.Files
       ( files
       , dirs
       ) where

import Control.Monad ((>=>), filterM)
import System.Directory (getDirectoryContents, canonicalizePath)
import System.FilePath ((</>))
import System.Posix.Files (isRegularFile, isDirectory, getFileStatus, FileStatus)

-- | List the regular files in a given a directory.
files :: FilePath -> IO [FilePath]
files = flip filteredLs isRegularFile

-- | List the directory names one level deep under a given parent directory.
dirs :: FilePath -> IO [FilePath]
dirs = flip filteredLs isDirectory

-- | Filter out '.' and '..' from a directory listing.
filterSpecial :: Monad m => [FilePath] -> m [FilePath]
filterSpecial = filterM (\x -> return $ x /= "." && x /= "..")

-- | List a filtered directory listing for a given path.
filteredLs :: FilePath -> (FileStatus -> Bool) -> IO [FilePath]
filteredLs x p = ls x >>= filterM (getFileStatus >=> return . p)

-- | List the absolute path for all files in a given directory.
ls :: FilePath -> IO [FilePath]
ls x = getDirectoryContents x >>= filterSpecial >>= absPaths
  where absPaths = mapM (canonicalizePath . (x </>))
