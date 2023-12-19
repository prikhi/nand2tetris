{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

-- | Generate assembly code from our parsed VM types.
module VMTranslator.CodeGen (generateAssembly) where

import Assembler.Types hiding (BinaryOp (..), Constant, UnaryOp (..))
import Assembler.Types qualified as A
import Data.Bifunctor (second)
import Data.List qualified as L
import Data.Word (Word16)
import VMTranslator.Types


-- | Given a parsed list of VM commands, return the assembly that performs
-- these commands on the Hack hardware.
--
-- Each set of instructions will have it's original VM command prepended in
-- a comment.
--
-- We take in the basename of the file we are generating assembly for. This
-- lets us generate symbol addresses for each Static segment index.
generateAssembly :: FilePath -> [(String, Command)] -> [AssemblyLine]
generateAssembly fileName cs =
    let (_, concat . reverse -> assemblyCode) =
            L.foldl' (generateCommand fileName) (initialDynamicLabelCounter, []) cs
     in assemblyCode <> endLoop


generateCommand :: FilePath -> (DynamicLabelCounter, [[AssemblyLine]]) -> (String, Command) -> (DynamicLabelCounter, [[AssemblyLine]])
generateCommand fileName (nextDyanmicLabel, generated) (original, command) = case command of
    StackCommand c ->
        ( nextDyanmicLabel
        , toLines (generateStackCommand fileName c) : generated
        )
    ArithLogicCommand c ->
        second ((: generated) . toLines) $ generateArithLogicCommand nextDyanmicLabel c
  where
    toLines :: [Instruction] -> [AssemblyLine]
    toLines instrs =
        AssemblyComment (" " <> original) : map InstructionLine instrs


-- | Generate the assembly for the push & pop commands.
--
-- Pointer segments are special, they directly manipulate the base
-- addresses in @THIS@ & @THAT@ instead of the _values_ at the given
-- addresses.
--
-- Temp segments behave similarly, mapping the indexes to the special @R5@
-- to @R12@ symbols.
--
-- Static segments generate a symbol address for each index:
-- @<FileName>.<index>@. By converting the indexes to symbols, each file's
-- unique index can claim a register between 16 & 255.
--
-- For all other segments, we can emit optimized instructions for the case
-- where the index is zero, as the
generateStackCommand :: FilePath -> StackCommand -> [Instruction]
generateStackCommand fileName c = case c of
    Push segment index
        | segment == Pointer ->
            -- Pushing a pointer segment means pushing the address itself,
            -- not the value at the address:
            --
            -- > @THIS
            -- > D = M
            -- > @SP
            -- > M = M+1
            -- > A = M-1
            -- > M = D
            [ getPointerAddress index
            , set [D] $ A.Constant M
            ]
                <> pushDToStack
        | segment == Temp ->
            -- Temp segments behave like pointers.
            [ getTempAddress index
            , set [D] $ A.Constant M
            ]
                <> pushDToStack
        | segment == Static ->
            -- Each index of static segments have their own symbol, so we
            -- can fetch & push via the symbol address:
            --
            -- > @<FileName>.<index>
            -- > D = M
            -- > @SP
            -- > MA = M+1
            -- > A = M-1
            -- > M = D
            [ getStaticAddress index
            , set [D] $ A.Constant M
            ]
                <> pushDToStack
        | segment == Constant ->
            -- For pushing constants we can just do:
            --
            -- > @<index>
            -- > D = A
            -- > @SP
            -- > M = M+1
            -- > A = M-1
            -- > M = D
            [ constantAddress index
            , set [D] $ A.Constant A
            ]
                <> pushDToStack
        | index < 0 ->
            error $ "generateStackCommand: Negative indexes are disallowed: " <> show c
        | index == 0 ->
            -- When the index is 0, we can do a simple read & push:
            --
            -- > @segment
            -- > A = M
            -- > D = M
            -- > @SP
            -- > MA = M + 1
            -- > A = A - 1
            -- > M = D
            [ getOtherAddress segment
            , set [A] $ A.Constant M
            , set [D] $ A.Constant M
            ]
                <> pushDToStack
        | otherwise ->
            -- To push arbitrary segment locations, we need to do:
            --
            -- > @<index>
            -- > D = A
            -- > @<segment>
            -- > A = M + D
            -- > D = M
            -- > @SP
            -- > MA = M + 1
            -- > A = A - 1
            -- > M = D
            [ constantAddress index
            , set [D] $ A.Constant A
            , getOtherAddress segment
            , set [A] $ Binary M A.Add D
            , set [D] $ A.Constant M
            ]
                <> pushDToStack
    Pop segment index
        | segment == Constant ->
            error $ "generateStackCommand: Cannot pop to constant segment: " <> show c
        | index < 0 ->
            error $ "generateStackCommand: Negative indexes are disallowed: " <> show c
        | segment == Pointer ->
            -- Popping to a pointer segment means writing the stack value
            -- directly into THIS or THAT, not the value referenced by THIS
            -- or THAT:
            --
            -- > @SP
            -- > MA = M - 1
            -- > D = M
            -- > @THIS
            -- > M = D
            popToD
                <> [ getPointerAddress index
                   , set [M] $ A.Constant D
                   ]
        | segment == Temp ->
            -- Temp segments behave like pointers.
            popToD
                <> [ getTempAddress index
                   , set [M] $ A.Constant D
                   ]
        | segment == Static ->
            -- Each index of static segments have their own symbol, so we
            -- can pop & then write to the symbol address:
            --
            -- > @SP
            -- > MA = M - 1
            -- > D = M
            -- > @<FileName>.<index>
            -- > M = D
            popToD
                <> [ getStaticAddress index
                   , set [M] $ A.Constant D
                   ]
        | index == 0 ->
            -- When the index is 0, we can do a simple pop & write:
            --
            -- > @SP
            -- > MA = M - 1
            -- > D = M
            -- > @<segment>
            -- > A = M
            -- > M = D
            popToD
                <> [ getOtherAddress segment
                   , set [A] $ A.Constant M
                   , set [M] $ A.Constant D
                   ]
        | otherwise ->
            -- To pop into arbitrary segment locations, we need to save the
            -- target location to @R13@ to juggle the CPU registers between
            -- tracking the destination & the popped value:
            --
            -- > @index
            -- > D = A
            -- > @<segment>
            -- > D = D + M
            -- > @R13
            -- > M = D
            -- > @SP
            -- > MA = M - 1
            -- > D = M
            -- > @R13
            -- > A = M
            -- > M = D
            concat
                [
                    [ constantAddress index
                    , set [D] $ A.Constant A
                    , getOtherAddress segment
                    , set [D] $ Binary D A.Add M
                    , internalVMRegister
                    , set [M] $ A.Constant D
                    , stackPointer
                    ]
                , popToD
                ,
                    [ internalVMRegister
                    , set [A] $ A.Constant M
                    , set [M] $ A.Constant D
                    ]
                ]
  where
    getPointerAddress :: Word16 -> Instruction
    getPointerAddress =
        symbolAddress . \case
            0 -> "THIS"
            1 -> "THAT"
            _ ->
                error $
                    "generateStackCommand: Unsupported index for pointer segment: "
                        <> show c

    getTempAddress :: Word16 -> Instruction
    getTempAddress =
        symbolAddress . \case
            0 -> "R5"
            1 -> "R6"
            2 -> "R7"
            3 -> "R8"
            4 -> "R9"
            5 -> "R10"
            6 -> "R11"
            7 -> "R12"
            _ ->
                error $
                    "generateStackCommand: Unsupported index for temp segment: "
                        <> show c

    getStaticAddress :: Word16 -> Instruction
    getStaticAddress ix =
        symbolAddress $ fileName <> "." <> show ix

    getOtherAddress :: MemorySegment -> Instruction
    getOtherAddress = \case
        Argument -> symbolAddress "ARG"
        Local -> symbolAddress "LCL"
        This -> symbolAddress "THIS"
        That -> symbolAddress "THAT"
        _ ->
            error $
                "generateStackCommand: PROGRAMMER ERROR: attempted to call getOtherAddress for invalid segment: "
                    <> show c

    -- Push the value in the D register onto the stack:
    --
    -- > @SP
    -- > MA = M + 1
    -- > A = A - 1
    -- > M = D
    pushDToStack :: [Instruction]
    pushDToStack =
        [ stackPointer
        , set [M, A] $ Unary A.Increment M
        , set [A] $ Unary A.Decrement A
        , set [M] $ A.Constant D
        ]
    -- Pop the stack into the D register:
    --
    -- > @SP
    -- > MA = M - 1
    -- > D = M
    popToD :: [Instruction]
    popToD =
        [ stackPointer
        , set [M, A] $ Unary A.Decrement M
        , set [D] $ A.Constant M
        ]


generateArithLogicCommand :: DynamicLabelCounter -> ArithLogicCommand -> (DynamicLabelCounter, [Instruction])
generateArithLogicCommand counter = \case
    Neg -> performUnaryOp A.Negate
    Not -> performUnaryOp A.Not
    Add -> performNonComparisonBinaryOp A.Add
    Sub -> performNonComparisonBinaryOp A.Subtract
    And -> performNonComparisonBinaryOp A.And
    Or -> performNonComparisonBinaryOp A.Or
    Greater -> performComparisonOp JumpGT
    Equals -> performComparisonOp JumpEQ
    Less -> performComparisonOp JumpLT
  where
    -- Unary operations simply work on the top of stack item:
    --
    -- > @SP
    -- > A = M - 1
    -- > M = <op> M
    performUnaryOp :: A.UnaryOp -> (DynamicLabelCounter, [Instruction])
    performUnaryOp op =
        ( counter
        ,
            [ stackPointer
            , set [A] $ Unary A.Decrement M
            , set [M] $ Unary op M
            ]
        )

    -- Binary operations that are not comparisons are simple:
    --
    -- > @SP
    -- > MA = M - 1
    -- > D = M
    -- > A = A - 1
    -- > M = M <op> D
    performNonComparisonBinaryOp :: A.BinaryOp -> (DynamicLabelCounter, [Instruction])
    performNonComparisonBinaryOp op =
        ( counter
        ,
            [ stackPointer
            , set [M, A] $ Unary A.Decrement M
            , set [D] $ A.Constant M
            , set [A] $ Unary A.Decrement A
            , set [M] $ Binary M op D
            ]
        )

    -- Comparison operations are the most complex. We subtract the operands
    -- & use jumps to control whether the end result is a @0@ for the false
    -- case or a @-1@ for the true case:
    --
    -- > @SP
    -- > MA = M - 1
    -- > D = M
    -- > A = A - 1
    -- > D = M - D
    -- > M = 0
    -- > @VM_TRANSLATOR_CONDITIONAL_TRUE_<counter>
    -- > D;<jmp>
    -- > @SP
    -- > A = M - 1
    -- > M = 1
    -- > (VM_TRANSLATOR_CONDITIONAL_TRUE_<counter>)
    -- > @SP
    -- > A = M - 1
    -- > M = M - 1
    --
    -- The label is dynamically generated & we return an incremented
    -- counter.
    --
    -- Note that we subtract the top item of the stack from the 2nd item,
    -- so jump comparisons should be equivalent to the command operation.
    --
    -- TODO: is it possible to make this a single "@SP; A = M - 1" instead of two?
    performComparisonOp :: Jump -> (DynamicLabelCounter, [Instruction])
    performComparisonOp jump =
        let (nextCounter, label, labelAddress) = makeConditionalTrueLabel counter
         in ( nextCounter
            ,
                [ stackPointer
                , set [M, A] $ Unary A.Decrement M
                , set [D] $ A.Constant M
                , set [A] $ Unary A.Decrement A
                , set [D] $ Binary M A.Subtract D
                , set [M] Zero
                , AddressInstruction labelAddress
                , jumpOnD jump
                , stackPointer
                , set [A] $ Unary A.Decrement M
                , set [M] One
                , label
                , stackPointer
                , set [A] $ Unary A.Decrement M
                , set [M] $ Unary A.Decrement M
                ]
            )


-- HELPERS

constantAddress :: Word16 -> Instruction
constantAddress = AddressInstruction . ConstantAddress


symbolAddress :: String -> Instruction
symbolAddress = AddressInstruction . SymbolAddress


computeNoJump :: Maybe [Location] -> Computation -> Instruction
computeNoJump mbL c = ComputeInstruction mbL c Nothing


jumpOnD :: Jump -> Instruction
jumpOnD = ComputeInstruction Nothing (A.Constant D) . Just


set :: [Location] -> Computation -> Instruction
set ls = computeNoJump (Just ls)


makeConditionalTrueLabel :: DynamicLabelCounter -> (DynamicLabelCounter, Instruction, Address)
makeConditionalTrueLabel counter =
    let labelSymbol = makeDynamicLabelSymbol "VM_TRANSLATOR_CONDITIONAL_TRUE_" counter
     in ( succ counter
        , LabelInstruction labelSymbol
        , SymbolAddress labelSymbol
        )


-- DYNAMIC LABELS

-- | Stores the counter for generating dynamic labels w/ int suffixes
newtype DynamicLabelCounter = DynamicLabelCounter
    { fromDynamicLabelCounter :: Int
    }
    deriving newtype (Show, Enum, Bounded)


-- | Start the counter at @0@.
initialDynamicLabelCounter :: DynamicLabelCounter
initialDynamicLabelCounter = DynamicLabelCounter 0


-- | Combine the current counter value with the prefix string to generate
-- a label string.
makeDynamicLabelSymbol :: String -> DynamicLabelCounter -> String
makeDynamicLabelSymbol pfx counter = pfx <> show (fromDynamicLabelCounter counter)


-- CONSTANTS

-- | Go to the stack pointer
stackPointer :: Instruction
stackPointer = symbolAddress "SP"


-- | Go to @R13@, which is the first reserved slot for internal VM use.
internalVMRegister :: Instruction
internalVMRegister = symbolAddress "R13"


-- | End of program loop
endLoop :: [AssemblyLine]
endLoop =
    [ AssemblyComment " execution complete, loop forever"
    , InstructionLine $ LabelInstruction "VM_TRANSLATOR_END"
    , InstructionLine $ symbolAddress "VM_TRANSLATOR_END"
    , InstructionLine $ ComputeInstruction Nothing Zero (Just JumpJP)
    ]
