module Main where

import qualified Data.Text as T
import Data.Text.IO (readFile)
import Hack.Assembler.Parser
import Prelude hiding(readFile)
import System.Environment (getArgs)
import Text.Megaparsec ( parseTest )

main :: IO ()
main = do
  (filePath: _) <- getArgs
  p <- readFile filePath
  parseTest parser p
