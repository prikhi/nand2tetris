{-# LANGUAGE LambdaCase #-}

-- | Parser for assembly files
module Assembler.Parser where

import Assembler.Types
import Control.Monad (void)
import Data.Char (isAlphaNum, isDigit)
import Data.List qualified as L
import Data.Maybe (mapMaybe)
import Text.ParserCombinators.ReadP


-- | Parse lines from input string & return only instructions.
--
-- Print error on parse failure.
--
-- Note: we parse each line separately - this keeps the number of potential
-- parse trees for backtracking limited to one line of text, which keeps
-- memory usage down & speed up.
--
-- TODO: this could be in a @Utils.Parser@ module
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
parseLines :: ReadP [AssemblyLine]
parseLines = sepBy1 parseAssemblyLine newline <* optional newline


-- | Parse a single line of the file
parseAssemblyLine :: ReadP AssemblyLine
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
    parseBlankLine :: ReadP AssemblyLine
    parseBlankLine =
        look >>= \case
            [] -> pure AssemblyBlankLine
            ['\r'] -> get >> pure AssemblyBlankLine
            _ -> fail "parseBlankLine: Expected to see empty result or carriage return."
    parseComment :: ReadP AssemblyLine
    parseComment = do
        void $ string "//"
        AssemblyComment <$> munch (`notElem` "\r\n")
    parseAddressInstruction :: ReadP AssemblyLine
    parseAddressInstruction = do
        void $ char '@'
        InstructionLine . AddressInstruction <$> parseAddress
    parseComputeInstruction :: ReadP AssemblyLine
    parseComputeInstruction =
        fmap InstructionLine $
            ComputeInstruction
                <$> parseMaybe (many1 parseLocation <* spaceChars <* char '=' <* spaceChars)
                <*> parseComputation
                <*> parseMaybe parseJump
    parseLabelInstruction :: ReadP AssemblyLine
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
