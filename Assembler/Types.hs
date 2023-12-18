{-# LANGUAGE LambdaCase #-}

-- | Types generated during parsing of assembly files.
module Assembler.Types where

import Data.Word (Word16)


-- TYPES

data AssemblyLine
    = AssemblyComment String
    | AssemblyBlankLine
    | InstructionLine !Instruction
    deriving (Show)


-- | Extract potential instruction from line
getInstruction :: AssemblyLine -> Maybe Instruction
getInstruction = \case
    InstructionLine i -> Just i
    _ -> Nothing


data Instruction
    = AddressInstruction !Address
    | ComputeInstruction !(Maybe [Location]) !Computation !(Maybe Jump)
    | LabelInstruction !String
    deriving (Show)


data Address
    = SymbolAddress !String
    | ConstantAddress !Word16
    deriving (Show)


data Jump
    = JumpGT
    | JumpEQ
    | JumpGE
    | JumpLT
    | JumpNE
    | JumpLE
    | JumpJP
    deriving (Show)


data Computation
    = Zero
    | One
    | NegativeOne
    | Constant !Location
    | Unary !UnaryOp !Location
    | Binary !Location !BinaryOp !Location
    deriving (Show)


data Location
    = A
    | M
    | D
    deriving (Show, Eq)


data UnaryOp
    = Not
    | Negate
    | Increment
    | Decrement
    deriving (Show)


data BinaryOp
    = Add
    | Subtract
    | And
    | Or
    deriving (Show)


renderBinaryOp :: BinaryOp -> String
renderBinaryOp = \case
    Add -> "+"
    Subtract -> "-"
    And -> "&"
    Or -> "|"
