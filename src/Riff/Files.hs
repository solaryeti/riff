{- |
Module      : Riff.Files
Description : Functions for working with file pairs and obtaining directory listings
Copyright   : (c) 2024 Steven Meunier
License     : BSD-style (see the file LICENSE)
-}
module Riff.Files
  ( -- * Types
    FilePair (..)
  , FilePairs

    -- * FilePairs
  , new
  , old
  , newExist
  , rename
  , filePairs
  , renameableFilePairs

    -- * Directory Listing
  , dirContents
  , dirs
  , files
  )
where

import Riff.Prelude
import Riff.Sanitize

import System.Directory
  ( getDirectoryContents
  , makeAbsolute
  )
import System.FilePath
  ( takeFileName
  , (</>)
  )
import System.Posix.Files
  ( FileStatus
  , fileExist
  , getFileStatus
  , isDirectory
  , isRegularFile
  )
import qualified System.Posix.Files as F
  ( rename
  )
import Text.Show

type OldFilePath = FilePath
type NewFilePath = FilePath

-- | 'FilePath's grouped as the old 'FilePath' and the new 'FilePath'
-- that the file can be renamed to.
data FilePair = FilePair !OldFilePath !NewFilePath

-- | List of FilePairs
type FilePairs = [FilePair]

instance Show FilePair where
  show (FilePair x y) = x <> " -> " <> takeFileName y

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

-- | Build a list of 'FilePair's by processing a list of 'FilePath's
-- with a given 'Transformer'.  Any 'FilePath's that are the same
-- after being transformed are filtered from the resulting
-- 'FilePairs'.
filePairs :: Transformer -> [FilePath] -> FilePairs
filePairs f xs = filter (not . namesSame) $ map mkFilePair xs
 where
  namesSame (FilePair x y) = x == y
  mkFilePair x = FilePair x (transform f x)

-- | Filter 'FilePairs' to remove the 'FilePair's that possess a new
-- file name where that file already exists and renaming the file
-- would clobber the existing file.
renameableFilePairs :: FilePairs -> IO FilePairs
renameableFilePairs = filterM (fmap not . newExist)

-- | List the directory names one level deep under a given parent directory.
dirs :: FilePath -> IO [FilePath]
dirs = flip filteredLs isDirectory

-- | List the regular files in a given a directory.
files :: FilePath -> IO [FilePath]
files = flip filteredLs isRegularFile

-- | List all the files in a given directory with the regular files
-- preceding directories.
dirContents :: FilePath -> IO [FilePath]
dirContents x = (++) <$> files x <*> dirs x

-- | Filter out '.' and '..' from a directory listing.
filterSpecial :: [FilePath] -> IO [FilePath]
filterSpecial = filterM (\x -> return $ x /= "." && x /= "..")

-- | List a filtered directory listing for a given path.
filteredLs :: FilePath -> (FileStatus -> Bool) -> IO [FilePath]
filteredLs x p = ls x >>= filterM (fmap p . getFileStatus)

-- | List the absolute path for all files in a given directory.
ls :: FilePath -> IO [FilePath]
ls x = getDirectoryContents x >>= filterSpecial >>= absPaths
 where
  absPaths = mapM (makeAbsolute . (x </>))
