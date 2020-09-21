module Hack.VMTranslator.ParserTest where

import qualified Data.Text as T
import Hack.VMTranslator.Ast
import Hack.VMTranslator.Parser (parseVm)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec (parse)

cmdsWithExpected :: [(T.Text, Cmd)]
cmdsWithExpected =
  [ ("push constant 0", Push Constant 0),
    ("pop constant 1", Pop Constant 1),
    ("add", Add),
    ("eq", Eq),
    ("label hoge3._:", Label $ T.pack "hoge3._:"),
    ("function hoge3._: 0", Function (T.pack "hoge3._:") 0),
    ("return", Return)
  ]

test_single_cmd :: IO TestTree
test_single_cmd = do
  return $
    testGroup
      "parse with comment and empty line"
      $ fmap t cmdsWithExpected
  where
    t (cmd, expected) = testCase (T.unpack cmd) $ do
      case parseVm "" cmd of
        Right c -> do
          c @?= [expected]
        Left e -> do
          fail $ show e

test_with_scn :: IO TestTree
test_with_scn = do
  return $
    testGroup
      "parse with comment and empty line"
      $ fmap t cmdsWithExpected
  where
    t (cmd, expected) = testCase (T.unpack cmd) $ do
      let src = T.concat ["// comment\n", mappend cmd " // comment2\n", "\n", "add"]
      case parseVm "" src of
        Right cmds -> do
          cmds @?= [expected, Add]
        Left e -> do
          fail $ show e
