{-# LANGUAGE RecordWildCards #-}

module Hack.Assembler.Parser (parser) where

import Control.Monad (void)
import qualified Data.Text as T
import Data.Void
import Hack.Assembler.Ast
import Text.Megaparsec hiding (Label, label)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

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
  _ <- char '@'
  value <- Label <$> label <|> Const <$> constant
  return $ value

destP :: Parser Dest
destP = do
  choice
    [ AMD <$ string "AMD"
    , AM <$ string "AM"
    , AD <$ string "AD"
    , MD <$ string "MD"
    , single
    ]
  where single = choice [M <$ char 'M', D <$ char 'D', A <$ char 'A']

jumpP :: Parser Jump
jumpP = do
  choice $
    fmap (\j -> j <$ string (T.pack $ show j)) [ JGT, JEQ, JGE, JLT, JNE, JLE, JMP ]

binOp :: Comp -> Parser Char -> Parser Char -> Parser Char -> Parser Comp
binOp cons left op right = cons <$ (try $ do
  left
  op
  right)

compP :: Parser Comp
compP = do
  choice
    [ binOp DPlusOne d plus one
    , binOp APlusOne a plus one
    , binOp MPlusOne m plus one
    , binOp DMinusOne d minus one
    , binOp AMinusOne a minus one
    , binOp MMinusOne m minus one
    , binOp DPlusA d plus a
    , binOp DMinusA d minus a
    , binOp AMinusD a minus d
    , binOp DAndA d and a
    , binOp DOrA d or d
    , binOp DPlusM d plus m
    , binOp DMinusM d minus m
    , binOp MMinusD m minus d
    , binOp DAndM d and m
    , binOp DOrM d or m
    , Zero <$ zero
    , Atom <$> atom
    , UnaryMinus <$> do
      minus
      atom
    , do
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
    d <- destP
    void $ char '='
    return d
  comp <- compP
  jump <- optional . try $ do
    void $ char ';'
    jumpP
  return CInst_ {..}

inst :: Parser Inst
inst = do
  choice [AInst <$> aInst, CInst <$> cInst]

parser :: Parser Program
parser = many $ do
  i <- inst
  void newline <|> eof
  return i
