module Main where

import qualified Data.Text as T
import Hack.Assembler.Parser
import Text.Megaparsec

main :: IO ()
main = do
  l <- T.pack <$> getLine
  parseTest parser l
