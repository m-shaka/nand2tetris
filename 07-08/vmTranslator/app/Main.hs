module Main where

import Control.Monad (forM_, when)
import qualified Data.List as L
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Text.IO (appendFile, readFile, writeFile)
import Hack.VMTranslator.CodeWriter
import Hack.VMTranslator.Parser
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, removeFile)
import System.Environment (getArgs)
import System.FilePath
import Prelude hiding (appendFile, readFile, writeFile)

main :: IO ()
main = do
  (filePath : _) <- getArgs
  isDir <- doesDirectoryExist filePath
  (files, outPath) <-
    if isDir
      then do
        files <-
          fmap (\p -> joinPath [filePath, p])
            . L.filter (L.isSuffixOf ".vm")
            <$> listDirectory filePath
        let baseName = L.last $ splitDirectories filePath
        pure (files, joinPath [filePath, baseName ++ ".asm"])
      else do
        let dir = takeDirectory filePath
        pure ([filePath], joinPath [dir, takeBaseName filePath ++ ".asm"])
  writeFile outPath bootStrap
  forM_ files $ \path -> do
    p <- readFile $ path
    let baseName = takeBaseName path
    case parseVm filePath p of
      Right p -> do
        appendFile outPath $ generate p baseName
        appendFile outPath "\n"
      Left e -> putStrLn $ show e
