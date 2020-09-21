module Hack.VMTranslator.Parser (parseVm) where

import Control.Monad (void)
import qualified Data.Text as T
import Data.Void (Void)
import Hack.VMTranslator.Ast
import Text.Megaparsec hiding (Label, label)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

lineComment :: Parser ()
lineComment = L.skipLineComment "//"

scn :: Parser ()
scn = void $ L.space (space1 <|> void crlf) lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme scn

zeroArgCtr :: Show s => s -> Parser s
zeroArgCtr seg = lexeme $ seg <$ (string . T.toLower . T.pack . show $ seg)

segment :: Parser Segment
segment =
  lexeme $
    choice $
      fmap
        zeroArgCtr
        [Argument, Local, Static, Constant, This, That, Pointer, Temp]

identifier :: Parser T.Text
identifier = cons <$> head <*> (many $ head <|> digitChar)
  where
    head = choice [letterChar, char '_', char '.', char ':']
    cons h r = T.pack $ h : r

cmdParser :: Parser Cmd
cmdParser =
  choice $
    [ stackCmd "push" Push,
      stackCmd "pop" Pop,
      flow "label" Label,
      flow "goto" Goto,
      flow "if-goto" IfGoto,
      functionCall "function" Function,
      functionCall "call" Call,
      Return <$ (lexeme $ string "return")
    ]
      ++ fmap
        zeroArgCtr
        [Add, Sub, Neg, Eq, Gt, Lt, And, Or, Not]
  where
    stackCmd :: T.Text -> (Segment -> Int -> Cmd) -> Parser Cmd
    stackCmd cmd ctr = do
      void . lexeme $ string cmd
      seg <- lexeme segment
      index <- lexeme L.decimal
      return $ ctr seg index
    flow :: T.Text -> (T.Text -> Cmd) -> Parser Cmd
    flow cmd ctr = ctr <$> ((lexeme $ string cmd) *> lexeme identifier)
    functionCall :: T.Text -> (T.Text -> Int -> Cmd) -> Parser Cmd
    functionCall cmd ctr = do
      void . lexeme $ string cmd
      ctr <$> lexeme identifier <*> lexeme L.decimal

vmParser :: Parser [Cmd]
vmParser = many $ scn *> lexeme cmdParser

parseVm :: String -> T.Text -> Either (ParseErrorBundle T.Text Void) [Cmd]
parseVm srcName input = parse vmParser srcName input
