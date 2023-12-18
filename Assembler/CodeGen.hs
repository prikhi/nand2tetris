{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

-- | Generate machine code from our parsed assembly types.
module Assembler.CodeGen where

import Assembler.Types
import Data.Bits (testBit)
import Data.List qualified as L
import Data.Word (Word16)


-- | Given instructions, run a two-pass machine code generation.
--
-- First pass collects only C & A instructions while filling symbol table
-- with label locations.
--
-- Second pass uses symbol table to generate machine code for each
-- instruction.
generateInstructions :: [Instruction] -> [String]
generateInstructions instrs =
    let (_, labelSymbolTable, reverse -> nonLabelInstrs) =
            L.foldl'
                recordLabelLocations
                (0, initialSymbolTable, [])
                instrs
        (_, _, reverse -> binaryMachineCode) =
            L.foldl'
                generateMachineCode
                (16, labelSymbolTable, [])
                nonLabelInstrs
     in binaryMachineCode


-- | For each label instruction, add the symbol & next line to the table.
--
-- For all other instructions, increment the next line & keep the
-- instruction.
recordLabelLocations
    :: (Word16, [(String, Word16)], [Instruction])
    -> Instruction
    -> (Word16, [(String, Word16)], [Instruction])
recordLabelLocations (instrNum, symTable, seenInstrs) = \case
    LabelInstruction label ->
        (instrNum, (label, instrNum) : symTable, seenInstrs)
    i ->
        (succ instrNum, symTable, i : seenInstrs)


-- | Output the instruction as binary machine code.
--
-- If a symbol is referenced, check the table for it & claim the next
-- memory location is it does not exist.
generateMachineCode
    :: (Word16, [(String, Word16)], [String])
    -> Instruction
    -> (Word16, [(String, Word16)], [String])
generateMachineCode (nextSymbolLocation, symTable, output) = \case
    LabelInstruction sym ->
        error $ "generateMachineCode: Encountered label instruction for: " <> sym
    AddressInstruction (ConstantAddress num) ->
        (nextSymbolLocation, symTable, word16ToBinaryString num : output)
    AddressInstruction (SymbolAddress sym) ->
        case lookup sym symTable of
            Nothing ->
                ( succ nextSymbolLocation
                , (sym, nextSymbolLocation) : symTable
                , word16ToBinaryString nextSymbolLocation : output
                )
            Just symLoc ->
                ( nextSymbolLocation
                , symTable
                , word16ToBinaryString symLoc : output
                )
    ComputeInstruction mbDests computation mbJump ->
        ( nextSymbolLocation
        , symTable
        , concat
            [ "111"
            , generateComputation computation
            , maybe "000" generateDestination mbDests
            , maybe "000" generateJump mbJump
            ]
            : output
        )


generateComputation :: Computation -> String
generateComputation = \case
    Zero ->
        "0101010"
    One ->
        "0111111"
    NegativeOne ->
        "0111010"
    Constant D ->
        "0001100"
    Constant A ->
        "0110000"
    Constant M ->
        "1110000"
    Unary Not D ->
        "0001101"
    Unary Not A ->
        "0110001"
    Unary Not M ->
        "1110001"
    Unary Negate D ->
        "0001101"
    Unary Negate A ->
        "0110011"
    Unary Negate M ->
        "1110011"
    Unary Increment D ->
        "0011111"
    Unary Increment A ->
        "0110111"
    Unary Increment M ->
        "1110111"
    Unary Decrement D ->
        "0001110"
    Unary Decrement A ->
        "0110010"
    Unary Decrement M ->
        "1110010"
    Binary D Add A ->
        "0000010"
    Binary A Add D ->
        "0000010"
    Binary D Add M ->
        "1000010"
    Binary M Add D ->
        "1000010"
    Binary D Subtract A ->
        "0010011"
    Binary D Subtract M ->
        "1010011"
    Binary A Subtract D ->
        "0000111"
    Binary M Subtract D ->
        "1000111"
    Binary D And A ->
        "0000000"
    Binary A And D ->
        "0000000"
    Binary D And M ->
        "1000000"
    Binary M And D ->
        "1000000"
    Binary D Or A ->
        "0010101"
    Binary A Or D ->
        "0010101"
    Binary D Or M ->
        "1010101"
    Binary M Or D ->
        "1010101"
    Binary l1 op l2 ->
        error $
            "generateComputation: Unsupported Binary Operation: "
                <> concat [show l1, renderBinaryOp op, show l2]


generateDestination :: [Location] -> String
generateDestination ls =
    [ bitIfHasL A
    , bitIfHasL D
    , bitIfHasL M
    ]
  where
    bitIfHasL l = if l `elem` ls then '1' else '0'


generateJump :: Jump -> String
generateJump = \case
    JumpGT -> "001"
    JumpEQ -> "010"
    JumpGE -> "011"
    JumpLT -> "100"
    JumpNE -> "101"
    JumpLE -> "110"
    JumpJP -> "111"


word16ToBinaryString :: Word16 -> String
word16ToBinaryString i =
    map (\b -> if testBit i b then '1' else '0') [15, 14 .. 0]


initialSymbolTable :: [(String, Word16)]
initialSymbolTable =
    [ ("SP", 0)
    , ("LCL", 1)
    , ("ARG", 2)
    , ("THIS", 3)
    , ("THAT", 4)
    , ("SCREEN", 16384)
    , ("KBD", 24576)
    ]
        <> rSymbols
  where
    rSymbols :: [(String, Word16)]
    rSymbols = map (\i -> ("R" <> show i, i)) [0 .. 15]
