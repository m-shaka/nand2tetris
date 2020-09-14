module Hack.Assembler.Ast where

import Data.Text (Text)

data AInst
  = Label Text
  | Const Int
  deriving (Show)

data Dest
  = M
  | D
  | MD
  | A
  | AM
  | AD
  | AMD
  deriving (Show)

data Jump
  = JGT
  | JEQ
  | JGE
  | JLT
  | JNE
  | JLE
  | JMP
  deriving (Show)

data CompAtom
  = One
  | CA
  | CM
  | CD
  deriving (Show)

data Comp
  = Zero
  | Atom CompAtom
  | UnaryMinus CompAtom
  | NegA
  | NegM
  | NegD
  | DPlusOne
  | APlusOne
  | MPlusOne
  | DMinusOne
  | AMinusOne
  | MMinusOne
  | DPlusA
  | DMinusA
  | AMinusD
  | DAndA
  | DOrA
  | DPlusM
  | DMinusM
  | MMinusD
  | DAndM
  | DOrM
  deriving (Show)

type CInst = (Comp, Maybe Dest, Maybe Jump)

type LabelSymbol = Text

data Inst
  = AInst AInst
  | CInst CInst
  | LabelSymbol LabelSymbol
  deriving (Show)

type Program = [Inst]
