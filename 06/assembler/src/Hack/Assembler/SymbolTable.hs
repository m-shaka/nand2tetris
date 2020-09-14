module Hack.Assembler.SymbolTable (initializeTable, lookup, insertRAM, SymbolTable) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Hack.Assembler.Ast
import Prelude hiding (lookup)

data SymbolTable = SymbolTable (Map.Map T.Text Int) Int deriving (Show)

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

initializeTable :: Program -> SymbolTable
initializeTable p = SymbolTable table 16
  where
    (table, _) = foldl f (Map.fromList builtInSymbols, 0) p
    f (table', index) l = case l of
      LabelSymbol l -> (Map.insert l index table', index)
      _ -> (table', index + 1)

lookup :: T.Text -> SymbolTable -> Maybe Int
lookup key (SymbolTable table _) = Map.lookup key table

insertRAM :: T.Text -> SymbolTable -> (SymbolTable, Int)
insertRAM key (SymbolTable table nextRam) = (SymbolTable (Map.insert key nextRam table) (nextRam + 1), nextRam)
