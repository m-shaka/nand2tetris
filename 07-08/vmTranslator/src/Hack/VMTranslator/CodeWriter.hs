module Hack.VMTranslator.CodeWriter where

import Control.Monad.State
import qualified Data.Text as T
import Hack.VMTranslator.Ast

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

generateSingleCmd :: String -> Cmd -> State Int [T.Text]
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
  Eq -> compare "JEQ"
  Gt -> compare "JGT"
  Lt -> compare "JLT"
  And -> bin "D&M"
  Or -> bin "D|M"
  Not ->
    pure
      [ "@SP",
        "A=M-1",
        "M=!M"
      ]
  where
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

    compare jmp = do
      i <- get
      put $ i + 1
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
    pushD =
      [ "@SP",
        "A=M",
        "M=D",
        "@SP",
        "M=M+1"
      ]
    staticAInst n = T.concat ["@", T.pack fName, ".", T.pack . show $ n]

generate :: [Cmd] -> String -> T.Text
generate cmds fName =
  T.intercalate "\n" . concat $ res
  where
    (res, _) = runState (sequence $ fmap (generateSingleCmd fName) cmds) 0
