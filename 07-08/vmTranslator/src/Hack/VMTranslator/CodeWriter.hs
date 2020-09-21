module Hack.VMTranslator.CodeWriter (bootStrap, generate) where

import Control.Monad.State
import qualified Data.Text as T
import Hack.VMTranslator.Ast

type CurrentFunc = Maybe T.Text

type GeneratorState a = State (Int, CurrentFunc) a

segToName :: Segment -> T.Text
segToName Local = "LCL"
segToName Argument = "ARG"
segToName This = "THIS"
segToName That = "THAT"
segToName Pointer = "3"
segToName Temp = "5"

isMapToBaseSeg :: Segment -> Bool
isMapToBaseSeg Local = True
isMapToBaseSeg Argument = True
isMapToBaseSeg This = True
isMapToBaseSeg That = True
isMapToBaseSeg _ = False

generateSingleCmd :: String -> Cmd -> GeneratorState [T.Text]
generateSingleCmd fName cmd = case cmd of
  Push Constant n ->
    pure $
      [ mappend "@" $ T.pack . show $ n,
        "D=A"
      ]
        ++ pushD
  Push Static n ->
    pure $
      [ staticAInst n,
        "D=M"
      ]
        ++ pushD
  Push seg n ->
    pure $
      [ mappend "@" $ segToName seg,
        mappend "D=" $ if isMapToBaseSeg seg then "M" else "A",
        mappend "@" $ T.pack . show $ n,
        "A=D+A",
        "D=M"
      ]
        ++ pushD
  Pop Static n ->
    pure $
      [ "@SP",
        "M=M-1",
        "A=M",
        "D=M",
        staticAInst n,
        "M=D"
      ]
  Pop seg n ->
    pure
      [ mappend "@" $ segToName seg,
        mappend "D=" $ if isMapToBaseSeg seg then "M" else "A",
        mappend "@" $ T.pack . show $ n,
        "D=D+A",
        "@13",
        "M=D",
        "@SP",
        "M=M-1",
        "A=M",
        "D=M",
        "@13",
        "A=M",
        "M=D"
      ]
  Add ->
    bin "D+M"
  Sub ->
    bin "D-M"
  Neg ->
    pure
      [ "@SP",
        "A=M-1",
        "M=-M"
      ]
  Eq -> generateCompare "JEQ"
  Gt -> generateCompare "JGT"
  Lt -> generateCompare "JLT"
  And -> bin "D&M"
  Or -> bin "D|M"
  Not ->
    pure
      [ "@SP",
        "A=M-1",
        "M=!M"
      ]
  Label t ->
    pure [T.concat ["(", t, ")"]]
  Goto t -> do
    l <- gotoLabel t
    pure
      [ T.concat ["@", l],
        "0;JMP"
      ]
  IfGoto t -> do
    l <- gotoLabel t
    pure
      [ "@SP",
        "M=M-1",
        "A=M",
        "D=M",
        mappend "@" l,
        "D;JNE"
      ]
  Function name n -> do
    (i, _) <- get
    put $ (i, Just name)
    pure $
      [T.concat ["(", name, ")"]]
        ++ foldr (\n acc -> initLocal n ++ acc) [] [1 .. n]
  Return ->
    pure
      [ "@LCL",
        "D=M",
        "@13",
        "MD=D",
        "@5",
        "A=D-A",
        "D=M",
        "@14",
        "M=D",
        "@SP",
        "A=M-1",
        "D=M",
        "@ARG",
        "A=M",
        "M=D",
        "@ARG",
        "D=M",
        "@SP",
        "M=D+1",
        "@13",
        "A=M-1",
        "D=M",
        "@THAT",
        "M=D",
        "@13",
        "D=M",
        "@2",
        "A=D-A",
        "D=M",
        "@THIS",
        "M=D",
        "@13",
        "D=M",
        "@3",
        "A=D-A",
        "D=M",
        "@ARG",
        "M=D",
        "@13",
        "D=M",
        "@4",
        "A=D-A",
        "D=M",
        "@LCL",
        "M=D",
        "@14",
        "A=M",
        "0;JMP"
      ]
  Call name n -> generateCall name n
  where
    staticAInst n = T.concat ["@", T.pack fName, ".", T.pack . show $ n]

bin :: T.Text -> GeneratorState [T.Text]
bin calc =
  pure
    [ "@SP",
      "A=M-1",
      "A=A-1",
      "D=M",
      "A=A+1",
      mappend "D=" calc,
      "@SP",
      "M=M-1",
      "A=M-1",
      "M=D"
    ]

generateCompare :: T.Text -> GeneratorState [T.Text]
generateCompare jmp = do
  (i, f) <- get
  put $ (i + 1, f)
  let label = mappend "COMPARE_" $ T.pack . show $ i
      elseLabel = mappend label "_ELSE"
  pure
    [ "@SP",
      "A=M-1",
      "A=A-1",
      "D=M",
      "A=A+1",
      "D=D-M",
      mappend "@" label,
      mappend "D;" jmp,
      "@SP",
      "M=M-1",
      "A=M-1",
      "M=0",
      mappend "@" elseLabel,
      "0;JMP",
      T.concat ["(", label, ")"],
      "@SP",
      "M=M-1",
      "A=M-1",
      "M=-1",
      T.concat ["(", elseLabel, ")"]
    ]

pushD :: [T.Text]
pushD =
  [ "@SP",
    "A=M",
    "M=D",
    "@SP",
    "M=M+1"
  ]

gotoLabel :: T.Text -> GeneratorState T.Text
gotoLabel t = do
  (_, cf) <- get
  let suffix = case cf of
        Nothing -> ""
        Just f -> mappend "." f
  return $ mappend t ""

initLocal n =
  [ "@LCL",
    "D=M",
    mappend "@" $ T.pack . show $ n - 1,
    "A=D+A",
    "M=0"
  ]

pushForCall = foldr (\l acc -> [mappend "@" l, "D=M"] ++ pushD ++ acc) [] ["LCL", "ARG", "THIS", "THAT"]

generateCall name n = do
  l <- gotoLabel "RETURN"
  (i, f') <- get
  let returnLabel = T.concat [l, ".", name, ".", T.pack . show $ i]
  put $ (i + 1, f')
  pure $
    [ mappend "@" returnLabel,
      "D=A"
    ]
      ++ pushD
      ++ pushForCall
      ++ [ "@SP",
           "D=M",
           mappend "@" $ T.pack . show $ n,
           "D=D-A",
           "@5",
           "D=D-A",
           "@ARG",
           "M=D",
           "@SP",
           "D=M",
           "@LCL",
           "M=D",
           mappend "@" name,
           "0;JMP",
           T.concat ["(", returnLabel, ")"]
         ]

generate :: [Cmd] -> String -> T.Text
generate cmds fName =
  T.intercalate "\n" . concat $ res
  where
    (res, _) = runState (sequence $ fmap (generateSingleCmd fName) cmds) (0, Nothing)

bootStrap :: T.Text
bootStrap = mappend (T.intercalate "\n" $ ["@256", "D=A", "@SP", "M=D"] ++ callInit) "\n"
  where
    (callInit, _) = runState (generateCall "Sys.init" 0) (0, Nothing)
