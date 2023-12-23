{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Token types that we parse from jack files
module Compiler.Tokens where

import Data.Int (Int16)
import Data.Text (Text)


data Token
    = KeywordTok !KeywordTok
    | SymbolTok !SymbolTok
    | IntegerTok !Int16
    | StringTok !Text
    | IdentifierTok !Text
    | CommentTok !Text
    deriving (Show, Eq, Ord)


data KeywordTok
    = ClassTok
    | ConstructorTok
    | FunctionTok
    | MethodTok
    | FieldTok
    | StaticTok
    | VarTok
    | IntTok
    | CharTok
    | BooleanTok
    | VoidTok
    | TrueTok
    | FalseTok
    | NullTok
    | ThisTok
    | LetTok
    | DoTok
    | IfTok
    | ElseTok
    | WhileTok
    | ReturnTok
    deriving (Show, Eq, Ord, Enum, Bounded)


renderKeyword :: KeywordTok -> Text
renderKeyword = \case
    ClassTok -> "class"
    ConstructorTok -> "constructor"
    FunctionTok -> "function"
    MethodTok -> "method"
    FieldTok -> "field"
    StaticTok -> "static"
    VarTok -> "var"
    IntTok -> "int"
    CharTok -> "char"
    BooleanTok -> "boolean"
    VoidTok -> "void"
    TrueTok -> "true"
    FalseTok -> "false"
    NullTok -> "null"
    ThisTok -> "this"
    LetTok -> "let"
    DoTok -> "do"
    IfTok -> "if"
    ElseTok -> "else"
    WhileTok -> "while"
    ReturnTok -> "return"


data SymbolTok
    = OpenBraceTok
    | CloseBraceTok
    | OpenParenTok
    | CloseParenTok
    | OpenBrackTok
    | CloseBrackTok
    | DotTok
    | CommaTok
    | SemiColonTok
    | PlusTok
    | MinusTok
    | StarTok
    | SlashTok
    | AndTok
    | PipeTok
    | LessTok
    | GreaterTok
    | EqualTok
    | TildeTok
    deriving (Show, Eq, Ord, Enum, Bounded)


renderSymbol :: SymbolTok -> Char
renderSymbol = \case
    OpenBraceTok -> '{'
    CloseBraceTok -> '}'
    OpenParenTok -> '('
    CloseParenTok -> ')'
    OpenBrackTok -> '['
    CloseBrackTok -> ']'
    DotTok -> '.'
    CommaTok -> ','
    SemiColonTok -> ';'
    PlusTok -> '+'
    MinusTok -> '-'
    StarTok -> '*'
    SlashTok -> '/'
    AndTok -> '&'
    PipeTok -> '|'
    LessTok -> '<'
    GreaterTok -> '>'
    EqualTok -> '='
    TildeTok -> '~'
