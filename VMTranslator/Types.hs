{-# LANGUAGE LambdaCase #-}

-- | Types generated during parsing of VM files.
module VMTranslator.Types where

import Data.Word (Word16)


data VMLine
    = VMComment !String
    | VMBlankLine
    | VMCommand !Command
    deriving (Show)


getCommand :: VMLine -> Maybe Command
getCommand = \case
    VMCommand c -> Just c
    _ -> Nothing


-- | Virtual Memory Segments
data MemorySegment
    = Argument
    | Local
    | Static
    | Constant
    | This
    | That
    | Pointer
    | Temp
    deriving (Eq, Show)


-- | Commands of the VM Language
data Command
    = StackCommand !StackCommand
    | ArithLogicCommand !ArithLogicCommand
    | BranchCommand !BranchCommand
    | FunctionCommand !FunctionCommand
    deriving (Show)


data StackCommand
    = Push !MemorySegment !Word16
    | Pop !MemorySegment !Word16
    deriving (Show)


data ArithLogicCommand
    = Add
    | Sub
    | Neg
    | Equals
    | Greater
    | Less
    | And
    | Or
    | Not
    deriving (Show)


data BranchCommand
    = Label !String
    | Goto !String
    | IfGoto !String
    deriving (Show)


data FunctionCommand
    = Function !String !Word16
    | Return
    | Call !String !Word16
    deriving (Show)
