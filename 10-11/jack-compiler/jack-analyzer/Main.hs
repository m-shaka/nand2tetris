module Main where

import Control.Monad (forM_, when)
import qualified Data.List as L
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Text.IO (appendFile, readFile, writeFile)
import Jack.Ast
import Jack.Parser
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, removeFile)
import System.Environment (getArgs)
import System.FilePath
  ( joinPath,
    replaceExtension,
    splitDirectories,
    takeBaseName,
    takeDirectory,
  )
import Prelude hiding (appendFile, readFile, writeFile)

main :: IO ()
main = do
  (targetPath : _) <- getArgs
  let isFile = L.isSuffixOf ".jack" targetPath
  files <-
    if isFile
      then pure [targetPath]
      else do
        listFiles <- L.filter (L.isSuffixOf ".jack") <$> listDirectory targetPath
        pure $ fmap (\p -> joinPath [targetPath, p]) listFiles

  forM_ files $ \path -> do
    p <- readFile $ path
    case parseJack path p of
      Right p -> putStrLn $ show p
      Left e -> putStrLn $ show e
