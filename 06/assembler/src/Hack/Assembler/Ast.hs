module Hack.Assembler.Ast where

import Data.Text (Text)

data AInst
  = Label Text
  | Const Int
  deriving (Show)

data Inst
  = AInst AInst
  deriving (Show)

type Program = [Inst]
