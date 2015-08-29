-- | Working with file pairs and obtaining directory listings

module Riff.Files
       ( -- * Types
         FilePair(..)
       , FilePairs

         -- * FilePairs
       , new
       , old
       , newExist
       , rename

         -- * Directory Listing
       , dirs
       , files
       ) where

import Control.Monad ((>=>), filterM)
import System.Directory (getDirectoryContents, canonicalizePath)
import System.FilePath ((</>), takeFileName)
import System.Posix.Files (isRegularFile, isDirectory, getFileStatus, fileExist, FileStatus)
import qualified System.Posix.Files as F (rename)

type OldFilePath = FilePath
type NewFilePath = FilePath

-- | 'FilePath's grouped as the old 'FilePath' and the new 'FilePath'
-- that the file can be renamed to.
data FilePair = FilePair OldFilePath NewFilePath

-- | List of FilePairs
type FilePairs = [FilePair]

instance Show FilePair where
  show (FilePair x y) = x ++ " -> " ++ takeFileName y

-- | Return the new name in a 'FilePair'.
new :: FilePair -> FilePath
new (FilePair _ x) = x

-- | Return the old name in 'FilePair'.
old :: FilePair -> FilePath
old (FilePair x _) = x

-- | Check if a file or directory corresponding to the new name already exists.
newExist :: FilePair -> IO Bool
newExist (FilePair _ x) = fileExist x

-- | Rename the old file in a 'FilePair' to the new name.
rename :: FilePair -> IO ()
rename (FilePair x y) = F.rename x y

-- | List the directory names one level deep under a given parent directory.
dirs :: FilePath -> IO [FilePath]
dirs = flip filteredLs isDirectory

-- | List the regular files in a given a directory.
files :: FilePath -> IO [FilePath]
files = flip filteredLs isRegularFile

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
