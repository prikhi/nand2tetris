{-# LANGUAGE LambdaCase #-}

-- | Parse token stream into an AST type.
module Compiler.Parser
    ( parseAST
    ) where

import Compiler.AST
import Compiler.Lexer (SourceToken (..))
import Compiler.Tokens
import Control.Monad (void)
import Data.Int (Int16)
import Data.List.NonEmpty (fromList)
import Data.Text (Text)
import Text.Parsec


parseAST :: FilePath -> [SourceToken] -> Either ParseError Class
parseAST fp = parse (parseClass <* eof) fp . filter (notComment . tok)
  where
    notComment :: Token -> Bool
    notComment = \case
        CommentTok {} -> False
        _ -> True


-- TOKEN STREAM PARSER

type Parser = Parsec [SourceToken] ()


expect_ :: (SourceToken -> Maybe a) -> Parser a
expect_ = token show pos


expectKeyword :: KeywordTok -> Parser KeywordTok
expectKeyword kw = expect_ $ \st -> if tok st == KeywordTok kw then Just kw else Nothing


expectKeyword_ :: KeywordTok -> Parser ()
expectKeyword_ = void . expectKeyword


expectSymbol :: SymbolTok -> Parser SymbolTok
expectSymbol sym = expect_ $ \st -> if tok st == SymbolTok sym then Just sym else Nothing


expectSymbol_ :: SymbolTok -> Parser ()
expectSymbol_ = void . expectSymbol


expectIdentifier :: Parser Text
expectIdentifier = expect_ $ \st -> case tok st of
    IdentifierTok txt -> Just txt
    _ -> Nothing


expectInteger :: Parser Int16
expectInteger = expect_ $ \st -> case tok st of
    IntegerTok i -> Just i
    _ -> Nothing


expectString :: Parser Text
expectString = expect_ $ \st -> case tok st of
    StringTok txt -> Just txt
    _ -> Nothing


-- PARSING

parseClass :: Parser Class
parseClass = do
    expectKeyword_ ClassTok
    className <- expectIdentifier
    expectSymbol_ OpenBraceTok
    varDecs <- many parseClassVarDec
    subroutineDecs <- many parseSubroutineDec
    expectSymbol_ CloseBraceTok
    return $ Class className varDecs subroutineDecs


parseClassVarDec :: Parser ClassVarDec
parseClassVarDec = do
    constr <-
        choice
            [ StaticVar <$ expectKeyword StaticTok
            , FieldVar <$ expectKeyword FieldTok
            ]
    varType <- parseType
    varNames <- fromList <$> sepBy1 expectIdentifier (expectSymbol CommaTok)
    expectSymbol_ SemiColonTok
    return $ constr varType varNames


parseType :: Parser Type
parseType =
    choice
        [ CharTy <$ expectKeyword CharTok
        , IntTy <$ expectKeyword IntTok
        , BoolTy <$ expectKeyword BooleanTok
        , ClassTy <$> expectIdentifier
        ]


parseSubroutineDec :: Parser SubroutineDec
parseSubroutineDec = do
    constr <-
        choice
            [ ConstructorFunc <$ expectKeyword ConstructorTok
            , FunctionFunc <$ expectKeyword FunctionTok
            , MethodFunc <$ expectKeyword MethodTok
            ]
    typ <-
        choice
            [ Just <$> parseType
            , Nothing <$ expectKeyword VoidTok
            ]
    subroutineName <- expectIdentifier
    expectSymbol_ OpenParenTok
    paramList <- parseParameterList
    expectSymbol_ CloseParenTok
    constr typ subroutineName paramList <$> parseSubroutineBody


parseParameterList :: Parser ParameterList
parseParameterList =
    ParameterList
        <$> sepBy
            ( (,)
                <$> parseType
                <*> expectIdentifier
            )
            (expectSymbol CommaTok)


parseSubroutineBody :: Parser SubroutineBody
parseSubroutineBody = do
    expectSymbol_ OpenBraceTok
    localVars <- many parseLocalVarDec
    statements <- parseStatements
    expectSymbol_ CloseBraceTok
    return $ SubroutineBody localVars statements


parseLocalVarDec :: Parser LocalVarDec
parseLocalVarDec = do
    expectKeyword_ VarTok
    typ <- parseType
    varNames <- fromList <$> sepBy1 expectIdentifier (expectSymbol CommaTok)
    expectSymbol_ SemiColonTok
    return $ LocalVarDec typ varNames


parseStatements :: Parser [Statement]
parseStatements = many parseStatement


parseStatement :: Parser Statement
parseStatement =
    choice
        [ parseLet
        , parseIf
        , parseWhile
        , parseDo
        , parseReturn
        ]
  where
    parseLet :: Parser Statement
    parseLet = do
        expectKeyword_ LetTok
        varName <- expectIdentifier
        -- TODO: add optional array indeing here
        arrayIndexing <- optionMaybe $ do
            expectSymbol_ OpenBrackTok
            parseExpression <* expectSymbol_ CloseBrackTok
        expectSymbol_ EqualTok
        value <- parseExpression
        expectSymbol_ SemiColonTok
        return $ LetStatement varName arrayIndexing value
    parseIf :: Parser Statement
    parseIf = do
        expectKeyword_ IfTok
        expectSymbol_ OpenParenTok
        conditional <- parseExpression
        expectSymbol_ CloseParenTok
        expectSymbol_ OpenBraceTok
        trueStatements <- parseStatements
        expectSymbol_ CloseBraceTok
        falseStatements <- option [] $ do
            expectKeyword_ ElseTok
            expectSymbol_ OpenBraceTok
            stmts <- parseStatements
            expectSymbol_ CloseBraceTok
            return stmts
        return $ IfStatement conditional trueStatements falseStatements
    parseWhile :: Parser Statement
    parseWhile = do
        expectKeyword_ WhileTok
        expectSymbol_ OpenParenTok
        conditional <- parseExpression
        expectSymbol_ CloseParenTok
        expectSymbol_ OpenBraceTok
        whileBody <- parseStatements
        expectSymbol_ CloseBraceTok
        return $ WhileStatement conditional whileBody
    parseDo :: Parser Statement
    parseDo = do
        expectKeyword_ DoTok
        call <- parseSubroutineCall
        expectSymbol_ SemiColonTok
        return $ DoStatement call
    parseReturn :: Parser Statement
    parseReturn = do
        expectKeyword_ ReturnTok
        ReturnStatement <$> optionMaybe parseExpression <* expectSymbol_ SemiColonTok


parseSubroutineCall :: Parser SubroutineCall
parseSubroutineCall = do
    initialIdent <- expectIdentifier
    mbSubIdent <- optionMaybe (expectSymbol_ DotTok *> expectIdentifier)
    let constr = maybe (ThisSubroutineCall initialIdent) (ClassSubroutineCall initialIdent) mbSubIdent
    expectSymbol_ OpenParenTok
    exprs <- parseExpressionList
    expectSymbol_ CloseParenTok
    return $ constr exprs


parseExpressionList :: Parser ExpressionList
parseExpressionList =
    ExpressionList <$> sepBy parseExpression (expectSymbol_ CommaTok)


parseExpression :: Parser Expression
parseExpression =
    Expression
        <$> parseTerm
        <*> option
            []
            ( many $
                (,) <$> parseBinaryOp <*> parseTerm
            )


parseTerm :: Parser Term
parseTerm =
    choice
        [ IntTerm <$> expectInteger
        , StringTerm <$> expectString
        , KeywordTerm <$> parseConstant
        , UnaryOpTerm <$> parseUnaryOp <*> parseTerm
        , parseParenthesizedTerm
        , choice
            [ try $ SubroutineTerm <$> parseSubroutineCall
            , try parseArrayElementTerm
            , VarTerm <$> expectIdentifier
            ]
        ]
  where
    parseArrayElementTerm :: Parser Term
    parseArrayElementTerm = do
        ident <- expectIdentifier
        expectSymbol_ OpenBrackTok
        ixExpr <- parseExpression
        expectSymbol_ CloseBrackTok
        return $ ArrayElementTerm ident ixExpr
    parseParenthesizedTerm :: Parser Term
    parseParenthesizedTerm = do
        expectSymbol_ OpenParenTok
        expr <- parseExpression
        expectSymbol_ CloseParenTok
        return $ ParenthesizedTerm expr


parseConstant :: Parser Constant
parseConstant =
    choice
        [ TrueC <$ expectKeyword TrueTok
        , FalseC <$ expectKeyword FalseTok
        , NullC <$ expectKeyword NullTok
        , ThisC <$ expectKeyword ThisTok
        ]


parseUnaryOp :: Parser UnaryOp
parseUnaryOp =
    choice
        [ Negate <$ expectSymbol MinusTok
        , Not <$ expectSymbol TildeTok
        ]


parseBinaryOp :: Parser BinaryOp
parseBinaryOp =
    choice
        [ Add <$ expectSymbol PlusTok
        , Minus <$ expectSymbol MinusTok
        , Multiply <$ expectSymbol StarTok
        , Divide <$ expectSymbol SlashTok
        , And <$ expectSymbol AndTok
        , Or <$ expectSymbol PipeTok
        , LessThan <$ expectSymbol LessTok
        , GreaterThan <$ expectSymbol GreaterTok
        , Equals <$ expectSymbol EqualTok
        ]
