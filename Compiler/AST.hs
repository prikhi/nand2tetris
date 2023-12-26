-- | AST types from parsing the tokens.
module Compiler.AST where

import Data.Int (Int16)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)


-- | Compilation unit of a jack file. Each jack program is a series of
-- parsed Classes.
data Class
    = Class
        Text
        -- ^ Class Name
        [ClassVarDec]
        -- ^ 0 or more class variable declarations
        [SubroutineDec]
        -- ^ 0 or more function declarations
    deriving (Show)


data ClassVarDec
    = -- | 1 or more typed static variables
      StaticVar Type (NonEmpty Text)
    | -- | 1 or more typed field variables
      FieldVar Type (NonEmpty Text)
    deriving (Show)


-- | Type declarations for class or local variables
data Type
    = CharTy
    | IntTy
    | BoolTy
    | ClassTy Text
    deriving (Show)


-- | Function declarations for class subroutines
data SubroutineDec = SubroutineDec
    { subroutineType :: SubroutineType
    , returnType :: Maybe Type
    -- ^ Nothing is Void type
    , subroutineName :: Text
    -- ^ Name
    , parameters :: ParameterList
    -- ^ Args
    , subroutineBody :: SubroutineBody
    -- ^ Body Code
    }
    deriving (Show)


data SubroutineType
    = Constructor
    | Function
    | Method
    deriving (Show, Eq)


-- | Pairs of function argument types & identfiers
newtype ParameterList
    = ParameterList [(Type, Text)]
    deriving (Show)


-- | Body of subroutine declarations
data SubroutineBody
    = SubroutineBody [LocalVarDec] [Statement]
    deriving (Show)


-- | Local variable declarations
data LocalVarDec
    = LocalVarDec Type (NonEmpty Text)
    deriving (Show)


data Statement
    = -- | variable binding with optional array index expression
      LetStatement Text (Maybe Expression) Expression
    | -- | condition expression with true & false statement branches
      IfStatement Expression [Statement] [Statement]
    | -- | while conditional expression & statement body
      WhileStatement Expression [Statement]
    | -- | effectful function with no return binding
      DoStatement SubroutineCall
    | -- | return with optional value
      ReturnStatement (Maybe Expression)
    deriving (Show)


data Expression
    = -- | expression is a term chained with many binary ops on additioonal terms
      Expression Term [(BinaryOp, Term)]
    deriving (Show)


data Term
    = IntTerm Int16
    | StringTerm Text
    | KeywordTerm Constant
    | VarTerm Text
    | ArrayElementTerm Text Expression
    | ParenthesizedTerm Expression
    | UnaryOpTerm UnaryOp Term
    | SubroutineTerm SubroutineCall
    deriving (Show)


data Constant
    = TrueC
    | FalseC
    | NullC
    | ThisC
    deriving (Show)


data BinaryOp
    = Add
    | Minus
    | Multiply
    | Divide
    | And
    | Or
    | LessThan
    | GreaterThan
    | Equals
    deriving (Show)


data UnaryOp
    = Negate
    | Not
    deriving (Show)


data SubroutineCall
    = ThisSubroutineCall Text ExpressionList
    | ClassSubroutineCall Text Text ExpressionList
    deriving (Show)


newtype ExpressionList
    = ExpressionList [Expression]
    deriving (Show)
