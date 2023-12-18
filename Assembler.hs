{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Assembler where

import Control.Monad (void)
import Data.Bits (testBit)
import Data.Char (isAlphaNum, isDigit)
import Data.List qualified as L
import Data.Maybe (mapMaybe)
import Data.Word (Word16)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (readFile')
import Text.ParserCombinators.ReadP


-- MAIN

-- | Valiate args & run
main :: IO ()
main =
    getArgs >>= \case
        [asmFile] ->
            runAssembler asmFile
        args -> do
            putStrLn $
                unlines
                    [ "ERROR: Expected 1 argument, assembly file name, got: "
                    , ""
                    , "\t" <> show args
                    , ""
                    , "Assembler.hs <FILE_NAME>.asm"
                    , ""
                    , "Reads input file."
                    , "Translates to Hack Hack machine code."
                    , "Write results to <FILE_NAME>.hack in same directory as input file."
                    ]
            exitFailure


-- | Get input file text, parse it, generate the machine code, & write to
-- output file.
runAssembler :: FilePath -> IO ()
runAssembler asmPath = do
    inputText <- readFile' asmPath
    let parsedAssembly = runParser inputText
    let binaryInstructions = generateInstructions parsedAssembly
    let outputPath = stripExtension asmPath <> ".hack"
    writeFile outputPath $ unlines binaryInstructions
  where
    stripExtension :: FilePath -> FilePath
    stripExtension fp = case reverse fp of
        'm' : 's' : 'a' : '.' : rest ->
            reverse rest
        _ -> error "ERROR: Expected input file with extension '.asm'"


-- TYPES

data Line
    = Comment
    | Blank
    | InstructionLine !Instruction
    deriving (Show)


-- | Extract potential instruction from line
getInstruction :: Line -> Maybe Instruction
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


-- PARSER

-- | Parse lines from input string & return only instructions.
--
-- Print error on parse failure.
--
-- Note: we parse each line separately - this keeps the number of potential
-- parse trees for backtracking limited to one line of text, which keeps
-- memory usage down & speed up.
runParser :: String -> [Instruction]
runParser inputText =
    let parseResults = map (readP_to_S parseAssemblyLine) $ lines inputText
     in flip mapMaybe parseResults $ \parseResult ->
            case L.find (null . snd) parseResult of
                Just success ->
                    getInstruction $ fst success
                Nothing ->
                    error $
                        "runParser: Could not parse input line. Attempts:\n\t"
                            <> L.intercalate "\n\t" (map show parseResult)


-- | Parse all the lines of the file
parseLines :: ReadP [Line]
parseLines = sepBy1 parseAssemblyLine newline <* optional newline


-- | Parse a single line of the file
parseAssemblyLine :: ReadP Line
parseAssemblyLine =
    ( spaceChars
        *> ( parseComment
                <++ parseAddressInstruction
                <++ parseComputeInstruction
                <++ parseLabelInstruction
           )
        <* spaceChars
        <* optional (char '\r')
    )
        <++ parseBlankLine
  where
    parseBlankLine :: ReadP Line
    parseBlankLine =
        look >>= \case
            [] -> pure Blank
            ['\r'] -> get >> pure Blank
            _ -> fail "parseBlankLine: Expected to see empty result or carriage return."
    parseComment :: ReadP Line
    parseComment = do
        void $ string "//"
        void $ munch (`notElem` "\r\n")
        pure Comment
    parseAddressInstruction :: ReadP Line
    parseAddressInstruction = do
        void $ char '@'
        InstructionLine . AddressInstruction <$> parseAddress
    parseComputeInstruction :: ReadP Line
    parseComputeInstruction =
        fmap InstructionLine $
            ComputeInstruction
                <$> parseMaybe (many1 parseLocation <* spaceChars <* char '=' <* spaceChars)
                <*> parseComputation
                <*> parseMaybe parseJump
    parseLabelInstruction :: ReadP Line
    parseLabelInstruction = do
        symbol <- char '(' *> parseSymbol <* char ')'
        return . InstructionLine $ LabelInstruction symbol


parseAddress :: ReadP Address
parseAddress =
    choice
        [ ConstantAddress . read <$> munch1 isDigit
        , SymbolAddress <$> parseSymbol
        ]


parseSymbol :: ReadP String
parseSymbol = munch1 (\c -> isAlphaNum c || c == '$' || c == '.' || c == ':' || c == '_')


parseLocation :: ReadP Location
parseLocation =
    choice
        [ D <$ char 'D'
        , M <$ char 'M'
        , A <$ char 'A'
        ]


parseJump :: ReadP Jump
parseJump = do
    spaceChars
    void $ char ';'
    spaceChars
    choice $
        map
            (\(c, s) -> c <$ string s)
            [ (JumpGT, "JGT")
            , (JumpEQ, "JEQ")
            , (JumpGE, "JGE")
            , (JumpLT, "JLT")
            , (JumpNE, "JNE")
            , (JumpLE, "JLE")
            , (JumpJP, "JMP")
            ]


parseComputation :: ReadP Computation
parseComputation =
    choice
        [ Zero <$ char '0'
        , One <$ char '1'
        , NegativeOne <$ string "-1"
        , Constant <$> parseLocation
        , parseUnaryComputation
        , parseBinaryComputation
        ]


parseUnaryComputation :: ReadP Computation
parseUnaryComputation =
    choice
        [ Unary <$> parsePrefixOp <*> (spaceChars *> parseLocation)
        , do
            l <- parseLocation <* spaceChars
            op <- parsePostfixOp
            return $ Unary op l
        ]
  where
    -- parse prefix ops and @1 <op> <loc>@
    parsePrefixOp :: ReadP UnaryOp
    parsePrefixOp =
        choice
            [ Not <$ char '!'
            , Negate <$ char '-'
            , Increment <$ spaceChars <* char '1' <* spaceChars <* char '+'
            , Decrement <$ spaceChars <* char '1' <* spaceChars <* char '-'
            ]
    -- parse <loc> <op> 1
    parsePostfixOp :: ReadP UnaryOp
    parsePostfixOp = do
        op <-
            choice
                [ Increment <$ char '+'
                , Decrement <$ char '-'
                ]
        spaceChars <* char '1'
        return op


parseBinaryComputation :: ReadP Computation
parseBinaryComputation = do
    l1 <- parseLocation <* spaceChars
    op <- parseBinaryOp <* spaceChars
    Binary l1 op <$> parseLocation


parseBinaryOp :: ReadP BinaryOp
parseBinaryOp =
    choice
        [ Add <$ char '+'
        , Subtract <$ char '-'
        , And <$ char '&'
        , Or <$ char '|'
        ]


-- | Consume an optional carriage return & required newline
newline :: ReadP ()
newline = void $ optional (char '\r') >> char '\n'


-- | Consume space & tab characters
spaceChars :: ReadP ()
spaceChars = void $ munch (`elem` " \t")


-- | optionally run a parser
parseMaybe :: ReadP a -> ReadP (Maybe a)
parseMaybe p = option Nothing (Just <$> p)


-- GENERATOR

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
