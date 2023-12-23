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
    deriving (Show, Eq, Ord)


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
    deriving (Show, Eq, Ord)
