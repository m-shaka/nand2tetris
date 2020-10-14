module Jack.Ast where

import qualified Data.Text as T

type Identifier = T.Text

data UnaryOp
  = UnaryMinus
  | Not

instance Show UnaryOp where
  show UnaryMinus = "-"
  show Not = "~"

data BinOp
  = Add
  | Minus
  | Multi
  | Div
  | And
  | Or
  | GT'
  | LT'
  | Eq'

instance Show BinOp where
  show Add = "+"
  show Minus = "-"
  show Multi = "*"
  show Div = "/"
  show And = "&"
  show Or = "|"
  show GT' = ">"
  show LT' = "<"
  show Eq' = "="

data SubroutineCall = SubroutineCall (Maybe Identifier) Identifier [Expr] deriving (Show)

data Term
  = JackTrue
  | JackFalse
  | Null
  | This
  | IntConst Int
  | StringConst T.Text
  | VarName Identifier
  | ArrayVarIndexing Identifier Expr
  | SubroutineCallTerm SubroutineCall
  | WithUnaryOp UnaryOp Term
  | ParenExpr Expr
  deriving (Show)

data Expr
  = Term Term
  | WithBinOp Term BinOp Term
  deriving (Show)

data Statement
  = LetStmt Identifier (Maybe Expr) Expr
  | IfStmt Expr Statements Statements
  | WhileStmt Expr Statements
  | DoStmt SubroutineCall
  | ReturnStmt (Maybe Expr)
  deriving (Show)

newtype Statements = Statements [Statement] deriving (Show)

data SubroutineType
  = Constructor
  | Function
  | Method
  deriving (Show, Eq)

data Type
  = TInt
  | TChar
  | TBool
  | TVoid
  | ClassType ClassName
  deriving (Show)

type SubroutineName = Identifier

type VarName = Identifier

newtype VarDec = VarDec (Type, [VarName]) deriving (Show)

newtype SubroutineBody = SubroutineBody ([VarDec], Statements) deriving (Show)

newtype ParameterList = ParameterList [(Type, VarName)] deriving (Show)

newtype SubroutineDec
  = SubroutineDec (SubroutineType, Type, SubroutineName, ParameterList, SubroutineBody)
  deriving (Show)

data ClassVarType
  = Static
  | Field
  deriving (Show, Eq)

newtype ClassVarDec = ClassVarDec (ClassVarType, Type, [VarName]) deriving (Show)

type ClassName = Identifier

newtype Class = Class (ClassName, [ClassVarDec], [SubroutineDec]) deriving (Show)
