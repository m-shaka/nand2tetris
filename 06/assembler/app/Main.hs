module Main where

import qualified Data.Text as T
import Data.Text.IO (readFile)
import Hack.Assembler.Parser
import Prelude hiding(readFile)
import System.Environment (getArgs)

main :: IO ()
main = do
  (filePath: _) <- getArgs
  p <- readFile filePath
  case parseAsm filePath p of
    Right p -> putStrLn $ show p
    Left e -> putStrLn $ show e
