module Hack.Assembler.SymbolTable (initialTable, lookup, insertRAM, SymbolTable) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Prelude hiding (lookup)

data SymbolTable = SymbolTable (Map.Map T.Text Int) Int

builtInSymbols :: [(T.Text, Int)]
builtInSymbols =
  mappend
    [ ("SP", 0),
      ("LCL", 1),
      ("ARG", 2),
      ("THIS", 3),
      ("THAT", 4),
      ("SCREEN", 16384),
      ("KBD", 24576)
    ]
    (fmap (\n -> (mappend "R" (T.pack $ show n), n)) [0 .. 15])

initialTable :: SymbolTable
initialTable = SymbolTable table 16
  where
    table = Map.fromList builtInSymbols

lookup :: T.Text -> SymbolTable -> Maybe Int
lookup key (SymbolTable table _) = Map.lookup key table

insertRAM :: T.Text -> SymbolTable -> (SymbolTable, Int)
insertRAM key (SymbolTable table nextRam) = (SymbolTable (Map.insert key nextRam table) (nextRam + 1), nextRam)
