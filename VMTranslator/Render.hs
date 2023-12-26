{-# LANGUAGE LambdaCase #-}

-- | Render VM Commands as expected by the Emulator & Assembler
module VMTranslator.Render
    ( renderVMCommand
    ) where

import VMTranslator.Types


renderVMCommand :: Command -> String
renderVMCommand = \case
    StackCommand cmd -> renderStackCommand cmd
    ArithLogicCommand cmd -> renderArithLogicCommand cmd
    BranchCommand cmd -> renderBranchCommand cmd
    FunctionCommand cmd -> renderFunctionCommand cmd


renderStackCommand :: StackCommand -> String
renderStackCommand = \case
    Push seg ix ->
        unwords ["push", renderSegment seg, show ix]
    Pop seg ix ->
        unwords ["pop", renderSegment seg, show ix]


renderSegment :: MemorySegment -> String
renderSegment = \case
    Argument -> "argument"
    Local -> "local"
    Static -> "static"
    Constant -> "constant"
    This -> "this"
    That -> "that"
    Pointer -> "pointer"
    Temp -> "temp"


renderArithLogicCommand :: ArithLogicCommand -> String
renderArithLogicCommand = \case
    Add -> "add"
    Sub -> "sub"
    Neg -> "neg"
    Equals -> "eq"
    Greater -> "gt"
    Less -> "lt"
    And -> "and"
    Or -> "or"
    Not -> "not"


renderBranchCommand :: BranchCommand -> String
renderBranchCommand = \case
    Label l ->
        unwords ["label", l]
    Goto l ->
        unwords ["goto", l]
    IfGoto l ->
        unwords ["if-goto", l]


renderFunctionCommand :: FunctionCommand -> String
renderFunctionCommand = \case
    Return ->
        "return"
    Call name count ->
        unwords ["call", name, show count]
    Function name count ->
        unwords ["function", name, show count]
