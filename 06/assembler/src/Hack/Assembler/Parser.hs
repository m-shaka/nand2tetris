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

inst :: Parser Inst
inst = do
  AInst <$> aInst

parser :: Parser Program
parser = many $ do
  i <- inst
  void newline <|> eof
  return i
