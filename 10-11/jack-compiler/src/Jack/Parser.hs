module Jack.Parser (parseJack) where

import Control.Monad (void, when)
import qualified Data.Text as T
import Data.Void (Void)
import Jack.Ast
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

lineComment :: Parser ()
lineComment = L.skipLineComment "//"

scn :: Parser ()
scn = void $ L.space (space1 <|> void crlf) lineComment (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme scn

primitiveTypes :: [T.Text]
primitiveTypes = ["int", "boolean", "char"]

nonTypeKeywords :: [T.Text]
nonTypeKeywords =
  [ "class",
    "constructor",
    "method",
    "function",
    "void",
    "var",
    "static",
    "field",
    "let",
    "do",
    "if",
    "else",
    "while",
    "return",
    "true",
    "false",
    "null",
    "this"
  ]

keywords :: [T.Text]
keywords =
  primitiveTypes ++ nonTypeKeywords

identifier :: String -> [T.Text] -> Parser T.Text
identifier errMsg keywords = try . lexeme $ do
  i <- cons <$> head <*> (many $ head <|> digitChar)
  when (i `elem` keywords) $ fail errMsg
  pure i
  where
    head = choice [letterChar, char '_']
    cons h r = T.pack $ h : r

varName :: Parser T.Text
varName = identifier "keywords cannot be used as variable name." keywords

typeName :: Parser T.Text
typeName = identifier "keywords cannot be used as type name." nonTypeKeywords

semicolon :: Parser ()
semicolon = void $ char ';'

arrayVarWithIndexing :: Parser Term
arrayVarWithIndexing = try $ do
  n <- varName
  void . lexeme . char $ '['
  index <- lexeme expr
  void . char $ ']'
  pure $ ArrayVarIndexing n index

stringInst :: Parser Term
stringInst = do
  void $ char '"'
  t <- takeWhileP (Just "any char excect new line and double quote.") (\t -> (t /= '"' && t /= '\n'))
  void $ char '"'
  pure $ StringConst t

term :: Parser Term
term =
  lexeme $
    choice
      [ SubroutineCallTerm <$> subroutineCall,
        arrayVarWithIndexing,
        VarName <$> varName,
        This <$ string "this",
        Null <$ string "null",
        JackTrue <$ string "true",
        JackFalse <$ string "false",
        WithUnaryOp <$> unaryOp <*> term,
        IntConst <$> L.decimal,
        stringInst,
        do
          lexeme . void . char $ '('
          e <- expr
          void $ char ')'
          return $ ParenExpr e
      ]

unaryOp :: Parser UnaryOp
unaryOp =
  lexeme . choice $
    fmap
      (\op -> op <$ (string . T.pack . show $ op))
      [UnaryMinus, Not]

binOp :: Parser BinOp
binOp =
  lexeme . choice $
    fmap
      (\op -> op <$ (string . T.pack . show $ op))
      [Add, Minus, Multi, Div, And, Or, GT', LT', Eq']

subroutineCall :: Parser SubroutineCall
subroutineCall = try . lexeme $ do
  ctr <- choice [methodOrFunc, SubroutineCall Nothing <$> varName]
  void $ char '('
  exprs <- lexeme $ expr `sepBy` (lexeme $ char ',')
  void $ char ')'
  pure $ ctr exprs
  where
    methodOrFunc = try $ do
      classOrVar <- Just <$> varName
      void $ char '.'
      SubroutineCall classOrVar <$> varName

expr :: Parser Expr
expr = lexeme $ (try $ WithBinOp <$> term <*> binOp <*> term) <|> Term <$> term

letStmt :: Parser Statement
letStmt = lexeme $ do
  void . lexeme . string $ "let"
  name <- lexeme varName
  indexing <- try . optional . lexeme $ char '[' *> expr <* char ']'
  void . lexeme . char $ '='
  rightExpr <- lexeme expr
  semicolon
  pure $ LetStmt name indexing rightExpr

ifStmt :: Parser Statement
ifStmt = lexeme $ do
  void . lexeme . string $ "if"
  void . lexeme . char $ '('
  cond <- expr
  void . lexeme . char $ ')'
  void . lexeme . char $ '{'
  thenStmts <- lexeme statements
  void . lexeme . char $ '}'
  elseStmts <- try . option (Statements []) $ do
    void . lexeme . string $ "else"
    void . lexeme . char $ '{'
    stmts' <- lexeme statements
    void . lexeme . char $ '}'
    pure stmts'
  pure $ IfStmt cond thenStmts elseStmts

whileStmt :: Parser Statement
whileStmt = do
  void . lexeme . string $ "while"
  void . lexeme . char $ '('
  cond <- expr
  void . lexeme . char $ ')'
  void . lexeme . char $ '{'
  stmts <- lexeme statements
  void . lexeme . char $ '}'
  pure $ WhileStmt cond stmts

doStmt :: Parser Statement
doStmt = do
  void . lexeme . string $ "do"
  call <- DoStmt <$> lexeme subroutineCall
  semicolon
  pure call

returnStmt :: Parser Statement
returnStmt = do
  void . lexeme . string $ "return"
  stmt <- ReturnStmt <$> (try . optional $ expr)
  semicolon
  pure stmt

stmt :: Parser Statement
stmt =
  lexeme $
    choice
      [letStmt, ifStmt, whileStmt, doStmt, returnStmt]

statements :: Parser Statements
statements = Statements <$> many stmt

type' :: Parser Type
type' =
  lexeme $
    choice
      [ ClassType <$> typeName,
        TInt <$ string "int",
        TChar <$ string "char",
        TBool <$ string "boolean"
      ]

typeAndVars :: Parser (Type, [VarName])
typeAndVars = lexeme $ do
  t <- type'
  vars <- varName `sepBy` (lexeme $ char ',')
  semicolon
  pure (t, vars)

subroutineBody :: Parser SubroutineBody
subroutineBody = do
  void . lexeme . char $ '{'
  vars <- many varDec
  stmts <- statements
  void . lexeme . char $ '}'
  pure $ SubroutineBody (vars, stmts)
  where
    varDec = lexeme $ do
      void . lexeme . string $ "var"
      VarDec <$> typeAndVars

parameterList :: Parser ParameterList
parameterList = do
  ParameterList <$> p `sepBy` (lexeme $ char ',')
  where
    p = (,) <$> type' <*> varName

subroutineDec :: Parser SubroutineDec
subroutineDec = do
  st <- subroutineType
  returnType <- lexeme $ choice [ClassType <$> typeName, TVoid <$ string "void"]
  sn <- varName
  void . lexeme . char $ '('
  pl <- parameterList
  void . lexeme . char $ ')'
  body <- subroutineBody
  pure $ SubroutineDec (st, returnType, sn, pl, body)
  where
    subroutineType =
      lexeme $
        choice
          [ Constructor <$ string "constructor",
            Function <$ string "function",
            Method <$ string "method"
          ]

classVarDec :: Parser ClassVarDec
classVarDec = lexeme $ do
  cvt <- lexeme $ choice [Static <$ string "static", Field <$ string "field"]
  (t, vs) <- typeAndVars
  pure $ ClassVarDec (cvt, t, vs)

class' :: Parser Class
class' = do
  void scn
  lexeme $ do
    void . lexeme . string $ "class"
    cn <- varName
    void . lexeme . char $ '{'
    cvds <- many classVarDec
    sds <- many subroutineDec
    void . char $ '}'
    pure $ Class (cn, cvds, sds)

parseJack :: String -> T.Text -> Either (ParseErrorBundle T.Text Void) Class
parseJack srcName input = parse class' srcName input
