module Main where

import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Text.IO (readFile, writeFile)
import Hack.Assembler.Code
import Hack.Assembler.Parser (parseAsm)
import System.Environment (getArgs)
import Text.Regex
import Prelude hiding (readFile, writeFile)

main :: IO ()
main = do
  (filePath : _) <- getArgs
  p <- readFile filePath
  let regxp = mkRegex "(.+)(.asm)$"
      outPath = subRegex regxp filePath "\\1.hack"
  case parseAsm filePath p of
    Right p -> writeFile outPath $ generate p
    Left e -> putStrLn $ show e
