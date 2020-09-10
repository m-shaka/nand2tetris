module Main where

import Hack.Assembler.Parser
import Text.Megaparsec

main :: IO ()
main = do
  parseTest parser "@_h2oge\n"
  parseTest parser "@100"
