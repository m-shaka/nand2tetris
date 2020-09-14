module Hack.Assembler.Code (generate) where

import Data.Maybe (maybe)
import qualified Data.Text as T
import Hack.Assembler.Ast
import Hack.Assembler.SymbolTable
import Text.Printf
import Prelude hiding (lookup)

generateAInst i = T.pack $ printf "%.16b\n" i

generateDest :: Maybe Dest -> T.Text
generateDest = maybe "000" gen
  where
    gen d = case d of
      M -> "001"
      D -> "010"
      MD -> "011"
      A -> "100"
      AM -> "101"
      AD -> "110"
      AMD -> "111"

generateJump :: Maybe Jump -> T.Text
generateJump = maybe "000" gen
  where
    gen j = case j of
      JGT -> "001"
      JEQ -> "010"
      JGE -> "011"
      JLT -> "100"
      JNE -> "101"
      JLE -> "110"
      JMP -> "111"

generateComp :: Comp -> T.Text
generateComp c = case c of
  Zero -> "0101010"
  Atom One -> "0111111"
  UnaryMinus One -> "0111010"
  Atom CD -> "0001100"
  Atom CA -> "0110000"
  NegD -> "0001101"
  NegA -> "0110001"
  UnaryMinus CD -> "0001111"
  UnaryMinus CA -> "0110011"
  DPlusOne -> "0011111"
  APlusOne -> "0110111"
  DMinusOne -> "0001110"
  AMinusOne -> "0110010"
  DPlusA -> "0000010"
  DMinusA -> "0010011"
  AMinusD -> "0000111"
  DAndA -> "0000000"
  DOrA -> "0010101"
  Atom CM -> "1110000"
  NegM -> "1110001"
  UnaryMinus CM -> "1110011"
  MPlusOne -> "1110111"
  MMinusOne -> "1110010"
  DPlusM -> "1000010"
  DMinusM -> "1010011"
  MMinusD -> "1000111"
  DAndM -> "1000000"
  DOrM -> "1010101"

generateLine :: Inst -> SymbolTable -> (T.Text, SymbolTable)
generateLine inst table = case inst of
  AInst (Const c) -> (generateAInst c, table)
  AInst (Label l) -> case lookup l table of
    Just r -> (generateAInst r, table)
    Nothing ->
      let (updateTable, ram) = insertRAM l table
       in (generateAInst ram, updateTable)
  CInst (comp, dest, jump) ->
    (T.concat ["111", generateComp comp, generateDest dest, generateJump jump, "\n"], table)
  _ -> ("", table)

generate :: Program -> T.Text
generate p = T.concat $ filter ((/=) "") $ reverse res
  where
    f (acc, table) i =
      let (line, table') = generateLine i table
       in (line : acc, table')
    table = initializeTable p
    (res, _) = foldl f ([], table) p
