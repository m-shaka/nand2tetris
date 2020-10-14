module Jack.SymbolTable
  ( SymbolTable,
    TableValue,
    initFromClsVar,
    initSubroutine,
    insert,
    lookup,
  )
where

import qualified Data.Bimap as BM
import Data.Text (Text)
import Jack.Ast
import Prelude hiding (lookup)

refFromType :: Type -> Maybe Text
refFromType t = case t of
  ClassType n -> Just n
  _ -> Nothing

type TableValue = (Text, Int, Maybe Text)

type Table = BM.Bimap Text TableValue

newtype SymbolTable = SymbolTable (Table, Table) deriving (Show)

insert :: Table -> Text -> Text -> Type -> Table
insert tbl key t type' =
  BM.insert key (t, index, refFromType type') tbl
  where
    filteredTbl = BM.filter (\_ (vt, _, _) -> vt == t) tbl
    index =
      if BM.size filteredTbl == 0
        then 0
        else (let ((_, maxIndex, _), _) = BM.findMaxR filteredTbl in maxIndex + 1)

lookup :: Text -> SymbolTable -> Maybe TableValue
lookup key (SymbolTable (clsTable, srTable)) =
  case lookup' clsTable of
    Nothing -> lookup' srTable
    res -> res
  where
    lookup' = BM.lookup key

initFromClsVar :: [ClassVarDec] -> SymbolTable
initFromClsVar vars = SymbolTable (clsVarTable, BM.empty)
  where
    updateClsVarTable tbl (ClassVarDec (vart, type', names)) =
      let vart' = if vart == Static then "static" else "this"
       in foldl (\tbl name -> insert tbl name vart' type') tbl names
    clsVarTable = foldl updateClsVarTable BM.empty vars

initThis :: Bool -> Text -> Table -> Table
initThis isMethod clsName sub =
  if isMethod then insert sub "this" "argument" (ClassType clsName) else sub

initSubroutine :: Bool -> Text -> ParameterList -> [VarDec] -> SymbolTable -> SymbolTable
initSubroutine isMethod clsName (ParameterList params) vars (SymbolTable (cls, sub)) =
  SymbolTable (cls, fromParams $ foldl fromVarDec sub' vars)
  where
    fromVarDec tbl (VarDec (t, names)) =
      foldl (\tbl name -> insert tbl name "local" t) tbl names
    fromParams tbl = foldl (\tbl (t, name) -> insert tbl name "argument" t) tbl params
    sub' = initThis isMethod clsName sub
