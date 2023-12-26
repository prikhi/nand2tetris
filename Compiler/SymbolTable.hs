{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

-- | Store class- & local-scoped symbols along with types, segments, & indexes.
module Compiler.SymbolTable
    ( SymbolTable (..)
    , initializeSymbolTable
    , resetLocalTable
    , insertClassVar
    , insertLocalVar
    , getSymbol
    , getNextLabel
    , ClassSymbolType (..)
    , LocalSymbolType (..)
    , typeToMemorySegment
    ) where

import Compiler.AST (Type (..))
import Data.Text (Text)
import Data.Word (Word16)
import VMTranslator.Types (MemorySegment (..))


-- SYMBOL TYPES

data LocalSymbolType
    = LocalArg
    | LocalVar
    deriving (Show, Eq)


data ClassSymbolType
    = StaticField
    | InstanceField
    deriving (Show, Eq)


-- | Determine the VM Memory segment for a symbol type
typeToMemorySegment :: Either ClassSymbolType LocalSymbolType -> MemorySegment
typeToMemorySegment = \case
    Left StaticField -> Static
    Left InstanceField -> This
    Right LocalArg -> Argument
    Right LocalVar -> Local


-- SYMBOL TABLE

data SymbolTable = SymbolTable
    { className :: Text
    , staticCounter :: Word16
    , fieldCounter :: Word16
    , localCounter :: Word16
    , argCounter :: Word16
    , classTable :: [(Text, (Type, ClassSymbolType, Word16))]
    , localTable :: [(Text, (Type, LocalSymbolType, Word16))]
    , nextLabelIx :: Word16
    -- ^ Throw this here since everything gets the symbol table already...
    }
    deriving (Show)


-- | Create a blank table. Use when enter code generation a new class.
initializeSymbolTable :: Text -> SymbolTable
initializeSymbolTable className =
    SymbolTable
        { staticCounter = 0
        , fieldCounter = 0
        , localCounter = 0
        , argCounter = 0
        , classTable = []
        , localTable = []
        , nextLabelIx = 0
        , ..
        }


-- | Attempt to retrieve the symbol from the table, throwing an error if it
-- does not exist.
getSymbol :: Text -> SymbolTable -> Maybe (Type, Word16, MemorySegment)
getSymbol varName st =
    case lookup varName st.localTable of
        Just (varT, symT, ix) ->
            Just (varT, ix, typeToMemorySegment $ Right symT)
        Nothing -> case lookup varName st.classTable of
            Just (varT, symT, ix) ->
                Just (varT, ix, typeToMemorySegment $ Left symT)
            Nothing ->
                Nothing


getNextLabel :: SymbolTable -> (SymbolTable, Word16)
getNextLabel st =
    ( st {nextLabelIx = succ st.nextLabelIx}
    , st.nextLabelIx
    )


-- | Reset the local part of the symbol table. Use when entering code
-- generation for a new function, method, or constructor.
resetLocalTable :: SymbolTable -> SymbolTable
resetLocalTable st =
    st
        { localCounter = 0
        , argCounter = 0
        , localTable = []
        , nextLabelIx = 0
        }


-- | Insert a new class-scoped variable into the symbol table.
insertClassVar :: Text -> Type -> ClassSymbolType -> SymbolTable -> SymbolTable
insertClassVar varName varType symType st =
    let ix = if symType == StaticField then st.staticCounter else st.fieldCounter
     in st
            { classTable = (varName, (varType, symType, ix)) : st.classTable
            , staticCounter = if symType == StaticField then succ st.staticCounter else st.staticCounter
            , fieldCounter = if symType == InstanceField then succ st.fieldCounter else st.fieldCounter
            }


-- | Insert a new local-scoped variable into the symbol table.
insertLocalVar :: Text -> Type -> LocalSymbolType -> SymbolTable -> SymbolTable
insertLocalVar varName varType symType st =
    let ix = if symType == LocalArg then st.argCounter else st.localCounter
     in st
            { localTable = (varName, (varType, symType, ix)) : st.localTable
            , localCounter = if symType == LocalVar then succ st.localCounter else st.localCounter
            , argCounter = if symType == LocalArg then succ st.argCounter else st.argCounter
            }
