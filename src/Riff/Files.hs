module Riff.Files
       ( files
       , dirs
       ) where

import Control.Monad ((>=>), filterM)
import System.Directory (getDirectoryContents, canonicalizePath)
import System.FilePath ((</>))
import System.Posix.Files (isRegularFile, isDirectory, getFileStatus, FileStatus)

files :: FilePath -> IO [FilePath]
files = flip filteredLs isRegularFile

dirs :: FilePath -> IO [FilePath]
dirs = flip filteredLs isDirectory


filterSpecial :: Monad m => [FilePath] -> m [FilePath]
filterSpecial = filterM (\x -> return $ x /= "." && x /= "..")

filteredLs :: FilePath -> (FileStatus -> Bool) -> IO [FilePath]
filteredLs x p = ls x >>= filterM (getFileStatus >=> return . p)

ls :: FilePath -> IO [FilePath]
ls x = getDirectoryContents x >>= filterSpecial >>= absPaths
  where absPaths = mapM (canonicalizePath . (x </>))
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
