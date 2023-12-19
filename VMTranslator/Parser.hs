{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

-- | Parser for VM files
module VMTranslator.Parser (runParser) where

import Control.Monad (void)
import Data.Char (isDigit)
import Data.List qualified as L
import Data.Maybe (mapMaybe)
import Text.ParserCombinators.ReadP
import VMTranslator.Types


-- | Parse lines from input string & return any commands with their
-- original line.
--
-- Print error & exit on failure to parse a complete line.
--
-- Note: each line is parsed separately to keep the backtracking
-- & potential parse trees short.
--
-- TODO: duped here & 'Assembler.Parser'.
runParser :: String -> [(String, Command)]
runParser inputText =
    let parseResults = map (\l -> (l,) <$> readP_to_S parseVMLine l) $ lines inputText
     in flip mapMaybe parseResults $ \parseResult ->
            case L.find (null . snd . snd) parseResult of
                Just (filter (/= '\r') -> original, (result, _)) ->
                    (original,) <$> getCommand result
                Nothing ->
                    error $
                        "runParser: Could not parse input line. Attempts:\n\t"
                            <> L.intercalate "\n\t" (map show $ L.sortOn (length . snd) parseResult)


parseVMLine :: ReadP VMLine
parseVMLine =
    ( spaceChars
        *> ( parseComment
                <++ (VMCommand <$> parseCommand)
           )
        <* spaceChars
        <* munch (== '\r')
    )
        <++ parseBlankLine
  where
    -- TODO: these two can prob go in Utils too
    parseBlankLine :: ReadP VMLine
    parseBlankLine =
        look >>= \case
            [] -> pure VMBlankLine
            ['\r'] -> get >> pure VMBlankLine
            _ -> fail "parseBlankLine: Expected to see empty result or carriage return."
    parseComment :: ReadP VMLine
    parseComment = do
        void $ string "//"
        VMComment <$> munch (`notElem` "\r\n")


parseCommand :: ReadP Command
parseCommand =
    choice
        [ StackCommand <$> parseStackCommand
        , ArithLogicCommand <$> parseArithLogicCommand
        ]


parseStackCommand :: ReadP StackCommand
parseStackCommand = do
    constr <-
        choice
            [ Push <$ string "push"
            , Pop <$ string "pop"
            ]
            <* spaceChars
    segment <- parseMemorySegment <* spaceChars
    value <- read <$> many1 (satisfy isDigit)
    return $ constr segment value


parseArithLogicCommand :: ReadP ArithLogicCommand
parseArithLogicCommand =
    choice $
        map
            (\(c, s) -> c <$ string s)
            [ (Add, "add")
            , (Sub, "sub")
            , (Neg, "neg")
            , (Equals, "eq")
            , (Greater, "gt")
            , (Less, "lt")
            , (And, "and")
            , (Or, "or")
            , (Not, "not")
            ]


-- TODO: map c s to Utils
parseMemorySegment :: ReadP MemorySegment
parseMemorySegment =
    choice $
        map
            (\(c, s) -> c <$ string s)
            [ (Argument, "argument")
            , (Local, "local")
            , (Static, "static")
            , (Constant, "constant")
            , (This, "this")
            , (That, "that")
            , (Pointer, "pointer")
            , (Temp, "temp")
            ]


-- TODO: Utils.Parsers

-- | Consume space & tab characters
spaceChars :: ReadP ()
spaceChars = void $ munch (`elem` " \t")
