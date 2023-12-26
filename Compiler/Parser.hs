{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Parse token stream into an AST type.
module Compiler.Parser
    ( parseAST
    ) where

import Compiler.AST
import Compiler.Lexer (SourceToken (..))
import Compiler.Tokens
import Compiler.XmlWriter (escapeXmlVal)
import Control.Monad (void)
import Data.Bifunctor (second)
import Data.Int (Int16, Int64)
import Data.List.NonEmpty (fromList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Text.Parsec


-- | Parse the AST from a file's token stream, returning the top-level
-- Class declaration and lazily-constructed XML output.
parseAST :: FilePath -> [SourceToken] -> Either ParseError (Class, TL.Text)
parseAST fp =
    fmap (second (TB.toLazyText . snd))
        . runParser ((,) <$> parseClass <*> getState <* eof) (0, mempty) fp
        . filter (notComment . tok)
  where
    notComment :: Token -> Bool
    notComment = \case
        CommentTok {} -> False
        _ -> True


-- TOKEN STREAM PARSER

-- | We parse a stream of 'SourceToken' and track an indentation about and
-- Lazy Text builder.
type Parser = Parsec [SourceToken] (Int64, TB.Builder)


-- | Expect lets us provide a matching function on the token while also
-- appending the token XML to our parser state.
expect_ :: (SourceToken -> Maybe a) -> Parser a
expect_ =
    tokenPrimEx
        (displayToken . tok)
        (\_ st _ -> pos st)
        ( Just $ \_ st _ (indent, xml) ->
            (indent, xml <> mkTokenXml indent st)
        )
  where
    mkTokenXml :: Int64 -> SourceToken -> TB.Builder
    mkTokenXml i st =
        let (tagName, tagVal) = renderToken $ tok st
            (open, close) = makeOpenAndCloseTags tagName
         in mconcat
                [ makeIndentText i
                , open
                , " "
                , tagVal
                , " "
                , close
                , "\n"
                ]
    renderToken :: Token -> (TB.Builder, TB.Builder)
    renderToken = \case
        CommentTok {} -> ("comment", "")
        IntegerTok i -> ("integerConstant", TB.fromString (show i))
        StringTok s -> ("stringConstant", TB.fromText (escapeXmlVal s))
        IdentifierTok ident -> ("identifier", TB.fromText ident)
        KeywordTok kw -> ("keyword", TB.fromText (renderKeyword kw))
        SymbolTok sym -> ("symbol", TB.fromText (escapeXmlVal (T.singleton (renderSymbol sym))))


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


-- XML GENERATION

-- | Write open tag for parent, increase indentation, run child parser,
-- decrease indentation, write close tag for parent.
withParentNode :: TB.Builder -> Parser a -> Parser a
withParentNode node action = do
    (parentIndent, _) <- getState
    let (open, close) = makeOpenAndCloseTags node
        indentText = makeIndentText parentIndent
    modifyState $ \(indent, xml) -> (succ indent, xml <> indentText <> open <> "\n")
    val <- action
    modifyState $ \(indent, xml) -> (pred indent, xml <> indentText <> close <> "\n")
    return val


-- | Build the string of space characters for a given indentation level.
makeIndentText :: Int64 -> TB.Builder
makeIndentText indent = TB.fromLazyText $ TL.replicate (indent * 2) " "


-- | Build the open & close XML tags for a node.
makeOpenAndCloseTags :: TB.Builder -> (TB.Builder, TB.Builder)
makeOpenAndCloseTags node =
    ( mconcat ["<", node, ">"]
    , mconcat ["</", node, ">"]
    )


-- PARSING

parseClass :: Parser Class
parseClass = withParentNode "class" $ do
    expectKeyword_ ClassTok
    className <- expectIdentifier
    expectSymbol_ OpenBraceTok
    varDecs <- many parseClassVarDec
    subroutineDecs <- many parseSubroutineDec
    expectSymbol_ CloseBraceTok
    return $ Class className varDecs subroutineDecs


parseClassVarDec :: Parser ClassVarDec
parseClassVarDec = withParentNode "classVarDec" $ do
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
parseSubroutineDec = withParentNode "subroutineDec" $ do
    subroutineType <-
        choice
            [ Constructor <$ expectKeyword ConstructorTok
            , Function <$ expectKeyword FunctionTok
            , Method <$ expectKeyword MethodTok
            ]
    returnType <-
        choice
            [ Just <$> parseType
            , Nothing <$ expectKeyword VoidTok
            ]
    subroutineName <- expectIdentifier
    expectSymbol_ OpenParenTok
    parameters <- parseParameterList
    expectSymbol_ CloseParenTok
    subroutineBody <- parseSubroutineBody
    return SubroutineDec {..}


parseParameterList :: Parser ParameterList
parseParameterList =
    withParentNode "parameterList" $
        ParameterList
            <$> sepBy
                ( (,)
                    <$> parseType
                    <*> expectIdentifier
                )
                (expectSymbol CommaTok)


parseSubroutineBody :: Parser SubroutineBody
parseSubroutineBody = withParentNode "subroutineBody" $ do
    expectSymbol_ OpenBraceTok
    localVars <- many parseLocalVarDec
    statements <- parseStatements
    expectSymbol_ CloseBraceTok
    return $ SubroutineBody localVars statements


parseLocalVarDec :: Parser LocalVarDec
parseLocalVarDec = withParentNode "varDec" $ do
    expectKeyword_ VarTok
    typ <- parseType
    varNames <- fromList <$> sepBy1 expectIdentifier (expectSymbol CommaTok)
    expectSymbol_ SemiColonTok
    return $ LocalVarDec typ varNames


parseStatements :: Parser [Statement]
parseStatements = withParentNode "statements" $ many parseStatement


parseStatement :: Parser Statement
parseStatement =
    choice
        [ withParentNode "letStatement" parseLet
        , withParentNode "ifStatement" parseIf
        , withParentNode "whileStatement" parseWhile
        , withParentNode "doStatement" parseDo
        , withParentNode "returnStatement" parseReturn
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
    withParentNode "expressionList" $
        ExpressionList <$> sepBy parseExpression (expectSymbol_ CommaTok)


parseExpression :: Parser Expression
parseExpression =
    withParentNode "expression" $
        Expression
            <$> parseTerm
            <*> option
                []
                ( many $
                    (,) <$> parseBinaryOp <*> parseTerm
                )


parseTerm :: Parser Term
parseTerm =
    withParentNode "term" $
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
