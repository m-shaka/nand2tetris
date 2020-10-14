module Main where

import Control.Monad (forM_)
import qualified Data.List as L
import Data.Text.IO (readFile)
import Jack.Parser
import Jack.VMWriter (write)
import System.Directory (listDirectory)
import System.Environment (getArgs)
import System.FilePath (joinPath, replaceExtension)
import Text.Megaparsec.Error
import Prelude hiding (readFile)

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
      Right cls -> do
        let outPath = replaceExtension path "vm"
        putStrLn $ "compile " ++ outPath ++ "..."
        write outPath cls
      Left e -> putStrLn $ errorBundlePretty e
