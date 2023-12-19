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
generateAssembly :: [(String, Command)] -> [AssemblyLine]
generateAssembly cs =
    let (_, concat . reverse -> assemblyCode) =
            L.foldl' generateCommand (initialDynamicLabelCounter, []) cs
     in assemblyCode <> endLoop


generateCommand :: (DynamicLabelCounter, [[AssemblyLine]]) -> (String, Command) -> (DynamicLabelCounter, [[AssemblyLine]])
generateCommand (nextDyanmicLabel, generated) (original, command) = case command of
    StackCommand c ->
        ( nextDyanmicLabel
        , toLines (generateStackCommand c) : generated
        )
    ArithLogicCommand c ->
        second ((: generated) . toLines) $ generateArithLogicCommand nextDyanmicLabel c
  where
    toLines :: [Instruction] -> [AssemblyLine]
    toLines instrs =
        AssemblyComment (" " <> original) : map InstructionLine instrs


generateStackCommand :: StackCommand -> [Instruction]
generateStackCommand = \case
    Push segment index
        | segment == Constant ->
            -- For pushing constants we can just do:
            --
            -- > @<index>
            -- > D = A
            -- > @SP
            -- > M = M+1
            -- > A = M
            -- > A = A-1
            -- > M = D
            [ constantAddress index
            , set [D] $ A.Constant A
            , stackPointer
            , set [M, A] $ Unary A.Increment M
            , set [A] $ Unary A.Decrement A
            , set [M] $ A.Constant D
            ]
    c@(Pop segment _)
        | segment == Constant ->
            -- Popping to a constant is invalid
            error $ "generateStackCommand: Cannot pop to constant segment: " <> show c
    _ -> []


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


{-
-- | Text for label & jump address to set latest item to @-1@, our
-- equivalent of 'True'.
comparisonTrueSymbol :: String
comparisonTrueSymbol = "VM_TRANSLATOR_SET_ZERO"

-- | Text for label & jump address to set latest item to @0@, our
-- equivalent of 'False'.
comparisonFalseSymbol :: String
comparisonFalseSymbol = "VM_TRANSLATOR_COMPARISON_FALSE"

-- | Emit the Label & computation sections for conditional results.
--
-- The general form is:
--
-- > (<label>)
-- > @SP
-- > A = M - 1
-- > M = <val>
conditionalResults :: [AssemblyLine]
conditionalResults =
    [ AssemblyComment " comparison true, set last to -1"
    , InstructionLine $ LabelInstruction comparisonTrueSymbol
    , InstructionLine stackPointer
    , InstructionLine $ set [A] $ Unary A.Decrement M
    , InstructionLine $ set [M] $ NegativeOne
    , AssemblyComment
    ]
-}

-- | End of program loop
endLoop :: [AssemblyLine]
endLoop =
    [ AssemblyComment " execution complete, loop forever"
    , InstructionLine $ LabelInstruction "VM_TRANSLATOR_END"
    , InstructionLine $ symbolAddress "VM_TRANSLATOR_END"
    , InstructionLine $ ComputeInstruction Nothing Zero (Just JumpJP)
    ]
