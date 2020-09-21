module Hack.VMTranslator.Ast where

import qualified Data.Text as T

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
  | Label T.Text
  | Goto T.Text
  | IfGoto T.Text
  | Function T.Text Int
  | Call T.Text Int
  | Return
  deriving (Show, Eq)
