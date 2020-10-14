module Jack.VMWriter (write) where

import Control.Monad.RWS.Strict
import Data.Char (ord)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO
import Jack.Ast
import qualified Jack.SymbolTable as ST
import Prelude hiding (writeFile)

type CodeGenerator = RWS (ST.SymbolTable, Text) [Text] Int

intToText :: Int -> Text
intToText = T.pack . show

stackOp :: Text -> Text -> Int -> Text
stackOp op seg index = T.intercalate " " [op, seg, intToText index]

push :: Text -> Int -> Text
push = stackOp "push"

pop :: Text -> Int -> Text
pop = stackOp "pop"

cnst :: Text
cnst = "constant"

this :: Text
this = "this"

that :: Text
that = "that"

pointer :: Text
pointer = "pointer"

argument :: Text
argument = "argument"

temp :: Text
temp = "temp"

pushIdentifier :: ST.TableValue -> CodeGenerator ()
pushIdentifier (seg, index, _) = tell [push seg index]

pushStringConst :: Text -> [Text]
pushStringConst t = push cnst (T.length t) : "call String.new 1" : join (fmap helper $ T.unpack t)
  where
    helper c = [push cnst $ ord c, "call String.appendChar 2"]

writeSubroutineCall :: SubroutineCall -> CodeGenerator ()
writeSubroutineCall (SubroutineCall maybeClsOrVar id exprs) = do
  (table, thisCls) <- ask
  let lenArgs = length exprs
  case maybeClsOrVar of
    Nothing -> do
      tell [push pointer 0]
      forM_ exprs writeExpr
      tell [T.concat ["call ", thisCls, ".", id, " ", intToText $ lenArgs + 1]]
    Just clsOrVar -> case ST.lookup clsOrVar table of
      Nothing -> do
        forM_ exprs writeExpr
        tell [T.concat ["call ", clsOrVar, ".", id, " ", intToText $ lenArgs]]
      Just res@(_, _, Just clsName) -> do
        pushIdentifier res
        forM_ exprs writeExpr
        tell [T.concat ["call ", clsName, ".", id, " ", intToText $ lenArgs + 1]]

writeTerm :: Term -> CodeGenerator ()
writeTerm term = case term of
  JackTrue -> tell [push cnst 1, "neg"]
  JackFalse -> tell [push cnst 0]
  Null -> tell [push cnst 0]
  This -> tell [push pointer 0]
  IntConst n -> tell [push cnst n]
  StringConst t -> tell $ pushStringConst t
  VarName id -> do
    (table, _) <- ask
    let Just res = ST.lookup id table
    pushIdentifier res
  ArrayVarIndexing id expr -> do
    (table, _) <- ask
    let Just res = ST.lookup id table
    pushIdentifier res
    writeExpr expr
    tell ["add", pop pointer 1, push that 0]
  SubroutineCallTerm subCall -> writeSubroutineCall subCall
  WithUnaryOp Not t -> writeTerm t >> tell ["not"]
  WithUnaryOp UnaryMinus t -> writeTerm t >> tell ["neg"]
  ParenExpr expr -> writeExpr expr

writeExpr :: Expr -> CodeGenerator ()
writeExpr expr = case expr of
  Term t -> writeTerm t
  WithBinOp t1 op t2 -> do
    writeTerm t1
    writeTerm t2
    case op of
      Add -> tell ["add"]
      Minus -> tell ["sub"]
      And -> tell ["and"]
      Or -> tell ["or"]
      GT' -> tell ["gt"]
      LT' -> tell ["lt"]
      Eq' -> tell ["eq"]
      Multi -> tell ["call Math.multiply 2"]
      Div -> tell ["call Math.divide 2"]

writeStatement :: Statement -> CodeGenerator ()
writeStatement stmt = case stmt of
  LetStmt identifier Nothing expr -> do
    (table, _) <- ask
    let Just (seg, index, _) = ST.lookup identifier table
    writeExpr expr
    tell [pop seg index]
  LetStmt identifier (Just indexExpr) expr -> do
    (table, _) <- ask
    let Just res = ST.lookup identifier table
    pushIdentifier res
    writeExpr indexExpr
    tell ["add"]
    writeExpr expr
    tell [pop temp 0, pop pointer 1, push temp 0, pop that 0]
  IfStmt condExpr (Statements thenStmts) (Statements elseStmts) -> do
    (_, clsName) <- ask
    writeExpr condExpr
    labelIndex <- intToText <$> get
    let thenLabel = T.concat [clsName, ":label-then", labelIndex]
        elseLabel = T.concat [clsName, ":label-else", labelIndex]
    tell [T.concat ["if-goto", " ", thenLabel]]
    modify (+ 1)
    forM_ elseStmts writeStatement
    tell [T.concat ["goto", " ", elseLabel]]
    tell [T.concat ["label", " ", thenLabel]]
    forM_ thenStmts writeStatement
    tell [T.concat ["label", " ", elseLabel]]
  WhileStmt condExpr (Statements stmts') -> do
    (_, clsName) <- ask
    labelIndex <- intToText <$> get
    let condLabel = T.concat [clsName, ":label-start", labelIndex]
        endLabel = T.concat [clsName, ":label-end", labelIndex]
    modify (+ 1)
    tell [T.concat ["label", " ", condLabel]]
    writeExpr condExpr
    tell ["not"]
    tell [T.concat ["if-goto", " ", endLabel]]
    forM_ stmts' writeStatement
    tell [T.concat ["goto", " ", condLabel]]
    tell [T.concat ["label", " ", endLabel]]
  DoStmt subCall -> writeSubroutineCall subCall >> tell [pop "temp" 0]
  ReturnStmt (Just expr) -> writeExpr expr >> tell ["return"]
  ReturnStmt _ -> tell [push cnst 0, "return"]

writeSubroutineDec :: Text -> Int -> SubroutineDec -> CodeGenerator ()
writeSubroutineDec clsName numClsVars (SubroutineDec (sbType, _, sbName, params, (SubroutineBody (vars, Statements stmts)))) = do
  local readerFunc $ do
    tell [T.intercalate " " ["function", T.concat [clsName, ".", sbName], intToText numVars]]
    when isConstructor $ tell [push cnst numClsVars, "call Memory.alloc 1", pop pointer 0]
    when isMethod $ tell [push argument 0, pop pointer 0]
    mapM writeStatement stmts
    pure ()
  where
    isConstructor = sbType == Constructor
    isMethod = sbType == Method
    readerFunc (tbl, n) = (ST.initSubroutine isMethod clsName params vars tbl, n)
    numVars = foldl (\acc (VarDec (_, names)) -> acc + length names) 0 vars

write' :: Class -> [Text]
write' (Class (clsName, clsVarDecs, subroutineDecs)) =
  w
  where
    foldHelper acc (ClassVarDec (t, _, names)) = if t == Field then acc + length names else acc
    numField = foldl foldHelper 0 clsVarDecs
    (_, w) =
      evalRWS
        (mapM (writeSubroutineDec clsName numField) subroutineDecs)
        (ST.initFromClsVar clsVarDecs, clsName)
        0

write :: FilePath -> Class -> IO ()
write outPath cls = writeFile outPath . T.intercalate "\n" $ write' cls
