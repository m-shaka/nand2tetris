module Main where

import Control.Monad (forM_)
import qualified Data.List as L
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Text.IO (readFile, writeFile)
import Hack.VMTranslator.CodeWriter
import Hack.VMTranslator.Parser
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (getArgs)
import System.FilePath
import Prelude hiding (readFile, writeFile)

main :: IO ()
main = do
  (filePath : _) <- getArgs
  isDir <- doesDirectoryExist filePath
  files <-
    if isDir
      then fmap (\p -> joinPath [filePath, p]) . L.filter (L.isSuffixOf ".vm") <$> listDirectory filePath
      else pure [filePath]
  forM_ files $ \path -> do
    p <- readFile $ path
    let outPath = replaceExtension path ".asm"
        baseName = takeBaseName path
    case parseVm filePath p of
      Right p -> writeFile outPath $ generate p baseName
      Left e -> putStrLn $ show e
