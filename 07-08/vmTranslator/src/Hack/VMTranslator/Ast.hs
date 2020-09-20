module Hack.VMTranslator.Ast where

data Segment
  = Argument
  | Local
  | Static
  | Constant
  | This
  | That
  | Pointer
  | Temp
  deriving (Show, Eq)

data Cmd
  = Add
  | Sub
  | Neg
  | Eq
  | Gt
  | Lt
  | And
  | Or
  | Not
  | Push Segment Int
  | Pop Segment Int
  deriving (Show, Eq)
