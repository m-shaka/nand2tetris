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

cmdParser :: Parser Cmd
cmdParser = do
  cmd <-
    choice $
      [twoArgCmd "push" Push, twoArgCmd "pop" Pop]
        ++ fmap
          zeroArgCtr
          [Add, Sub, Neg, Eq, Gt, Lt, And, Or, Not]
  return cmd
  where
    twoArgCmd :: T.Text -> (Segment -> Int -> Cmd) -> Parser Cmd
    twoArgCmd cmd ctr = do
      void . lexeme $ string cmd
      seg <- lexeme segment
      index <- lexeme L.decimal
      return $ ctr seg index

vmParser :: Parser [Cmd]
vmParser = many $ scn *> lexeme cmdParser

parseVm :: String -> T.Text -> Either (ParseErrorBundle T.Text Void) [Cmd]
parseVm srcName input = parse vmParser srcName input
