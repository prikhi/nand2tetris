-- | Parse jack files into 'Token' streams
module Compiler.Lexer
    ( tokenize
    , SourceToken (..)
    ) where

import Compiler.Tokens
import Data.Int (Int16)
import Data.Text (Text, pack, unpack)
import Text.Parsec
import Text.Parsec.Text


data SourceToken = SourceToken
    { pos :: SourcePos
    , tok :: Token
    }
    deriving (Show, Eq, Ord)


tokenize :: FilePath -> Text -> Either ParseError [SourceToken]
tokenize = parse (spaces *> many1 parseToken <* eof)


parseLexeme :: Parser Token -> Parser SourceToken
parseLexeme tokParser = do
    pos <- getPosition
    tok <- tokParser
    spaces
    return SourceToken {pos, tok}


parseToken :: Parser SourceToken
parseToken =
    parseLexeme $
        choice
            [ StringTok <$> parseString
            , IntegerTok <$> parseInteger
            , try $ KeywordTok <$> parseKeyword <* notFollowedBy alphaNum
            , IdentifierTok <$> parseIdentifier <* notFollowedBy alphaNum
            , try $ CommentTok <$> parseComment
            , SymbolTok <$> parseSymbol
            ]


parseString :: Parser Text
parseString = do
    pack <$> between (char '"') (char '"') (many $ satisfy (/= '"'))


parseInteger :: Parser Int16
parseInteger = do
    digits <- many1 digit <* notFollowedBy digit
    return $ read digits


parseIdentifier :: Parser Text
parseIdentifier = do
    c <- letter
    rest <- many $ alphaNum <|> char '_'
    return $ pack $ c : rest


parseComment :: Parser Text
parseComment =
    pack
        <$> choice
            [ try $ string "//" *> manyTill anyChar endOfLine
            , try $ string "/*" *> manyTill anyChar (try $ string "*/")
            ]


parseSymbol :: Parser SymbolTok
parseSymbol =
    choice $
        map (\v -> v <$ char (renderSymbol v)) [minBound .. maxBound]


parseKeyword :: Parser KeywordTok
parseKeyword =
    try . choice $
        map (\v -> v <$ try (string $ unpack $ renderKeyword v)) [minBound .. maxBound]
