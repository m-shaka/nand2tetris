{-# LANGUAGE RecordWildCards #-}

module Hack.Assembler.Parser (parseAsm) where

import Control.Monad (void)
import qualified Data.Text as T
import Data.Void (Void)
import Hack.Assembler.Ast
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

label :: Parser T.Text
label = do
  headChar <- head
  restChars <- many $ head <|> digitChar
  return $ T.pack (headChar : restChars)
  where
    head = choice [letterChar, char '_', char '.', char '$']

constant :: Parser Int
constant = L.decimal

aInst :: Parser AInst
aInst = do
  _ <- lexeme $ char '@'
  value <- lexeme $ Label <$> label <|> Const <$> constant
  return $ value

destP :: Parser Dest
destP = do
  choice
    [ AMD <$ string "AMD",
      AM <$ string "AM",
      AD <$ string "AD",
      MD <$ string "MD",
      single
    ]
  where
    single = choice [M <$ char 'M', D <$ char 'D', A <$ char 'A']

jumpP :: Parser Jump
jumpP = do
  choice $
    fmap (\j -> j <$ string (T.pack $ show j)) [JGT, JEQ, JGE, JLT, JNE, JLE, JMP]

binOp :: Comp -> Parser Char -> Parser Char -> Parser Char -> Parser Comp
binOp cons left op right =
  cons
    <$ ( try $ do
           lexeme left
           lexeme op
           lexeme right
       )

compP :: Parser Comp
compP = do
  choice
    [ binOp DPlusOne d plus one,
      binOp APlusOne a plus one,
      binOp MPlusOne m plus one,
      binOp DMinusOne d minus one,
      binOp AMinusOne a minus one,
      binOp MMinusOne m minus one,
      binOp DPlusA d plus a,
      binOp DMinusA d minus a,
      binOp AMinusD a minus d,
      binOp DAndA d and a,
      binOp DOrA d or d,
      binOp DPlusM d plus m,
      binOp DMinusM d minus m,
      binOp MMinusD m minus d,
      binOp DAndM d and m,
      binOp DOrM d or m,
      Zero <$ zero,
      Atom <$> atom,
      UnaryMinus <$> do
        minus
        atom,
      do
        char '!'
        choice [NegA <$ a, NegM <$ m, NegD <$ d]
    ]
  where
    zero = char '0'
    one = char '1'
    a = char 'A'
    m = char 'M'
    d = char 'D'
    plus = char '+'
    minus = char '-'
    and = char '&'
    or = char '|'
    atom = choice [One <$ one, CA <$ a, CM <$ m, CD <$ d]

cInst :: Parser CInst
cInst = do
  dest <- optional . try $ do
    d <- lexeme destP
    void $ lexeme $ char '='
    return d
  comp <- lexeme compP
  jump <- optional . try $ do
    void $ lexeme $ char ';'
    lexeme jumpP
  return (comp, dest, jump)

inst :: Parser Inst
inst = do
  choice [AInst <$> aInst, CInst <$> cInst]

parser :: Parser Program
parser = many $ do
  scn
  i <- lexeme inst
  return i

parseAsm :: String -> T.Text -> Either (ParseErrorBundle T.Text Void) Program
parseAsm srcName input = parse parser srcName input
