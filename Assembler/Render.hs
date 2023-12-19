{-# LANGUAGE LambdaCase #-}

{- | Renders the internal assembly types.

Allows emitting of assembly files by building line values in code.
-}
module Assembler.Render (renderAssemblyLine) where

import Assembler.Types


renderAssemblyLine :: AssemblyLine -> String
renderAssemblyLine = \case
    AssemblyBlankLine -> ""
    AssemblyComment text -> "//" <> text
    InstructionLine instruction -> renderInstruction instruction


renderInstruction :: Instruction -> String
renderInstruction = \case
    LabelInstruction symbol -> "(" <> symbol <> ")"
    AddressInstruction (SymbolAddress symbol) -> "@" <> symbol
    AddressInstruction (ConstantAddress constant) -> "@" <> show constant
    ComputeInstruction mbDests computation mbJumps ->
        maybe "" renderDests mbDests <> renderComputation computation <> maybe "" renderJump mbJumps
  where
    -- while _we_ allow parsing destinations in any order, the course tools
    -- require this to be ordered
    renderDests :: [Location] -> String
    renderDests ls =
        concat $
            concat
                [ [show A | A `elem` ls]
                , [show D | D `elem` ls]
                , [show M | M `elem` ls]
                , ["="]
                ]

    renderJump :: Jump -> String
    renderJump jump =
        ";" <> case jump of
            JumpGT -> "JGT"
            JumpEQ -> "JEQ"
            JumpGE -> "JGE"
            JumpLT -> "JLT"
            JumpNE -> "JNE"
            JumpLE -> "JLE"
            JumpJP -> "JMP"


renderComputation :: Computation -> String
renderComputation = \case
    Zero -> "0"
    One -> "1"
    NegativeOne -> "-1"
    Constant loc -> show loc
    Unary op loc -> renderUnaryOp loc op
    Binary l1 op l2 -> show l1 <> renderBinaryOp op <> show l2
  where
    -- we parse Increment with the location on either side but course tools
    -- expect it to be first
    renderUnaryOp :: Location -> UnaryOp -> String
    renderUnaryOp loc = \case
        Not -> "!" <> show loc
        Negate -> "-" <> show loc
        Increment -> show loc <> "+1"
        Decrement -> show loc <> "-1"
