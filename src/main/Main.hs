module Main where

import Control.Monad (filterM)
import Data.Char (toLower)
import Riff.Files
import Riff.Sanitize
import System.FilePath (combine, splitFileName)
import System.Environment (getArgs)

type Directory = FilePath

data FileType = Dirs | Files

transform :: String -> String
transform = uncurry combine . mapSnd transformers . splitFileName
  where transformers = map toLower . removeDupUnderscore . removeInvalid

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (x, y) = (f x, y)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)

main :: IO ()
main = do
  args <- getArgs
  mapM_ run args

newNames :: FileType -> Directory -> IO [(FilePath, FilePath)]
newNames t p = filter (uncurry (/=)) <$> map (\x -> (x, transform x)) <$> f p
  where f = case t of
          Dirs -> dirs
          Files -> files

rename :: FileType -> Directory -> IO ()
rename t x = (map snd <$> newNames t x) >>= mapM_ putStrLn

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
