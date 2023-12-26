{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- | Generate Hack VM commands from a Jack AST.
module Compiler.CodeGen
    ( generateClass
    ) where

import Compiler.AST
import Compiler.SymbolTable
import Data.Char (ord)
import Data.List qualified as L
import Data.Text (Text, unpack)
import Data.Word (Word16)
import VMTranslator.Types (MemorySegment (..))
import VMTranslator.Types qualified as VM


generateClass :: Class -> [VM.Command]
generateClass (Class className classVarDecs subroutineDecs) =
    let initialSymbolTable = initializeSymbolTable className
        classSymbolTable = L.foldl' insertClassSymbol initialSymbolTable classVarDecs
     in concatMap (generateSubroutineDec classSymbolTable) subroutineDecs
  where
    insertClassSymbol :: SymbolTable -> ClassVarDec -> SymbolTable
    insertClassSymbol st = \case
        StaticVar varT names ->
            L.foldl' (\st' name -> insertClassVar name varT StaticField st') st names
        FieldVar varT names ->
            L.foldl' (\st' name -> insertClassVar name varT InstanceField st') st names


generateSubroutineDec :: SymbolTable -> SubroutineDec -> [VM.Command]
generateSubroutineDec
    classSymbolTable
    SubroutineDec
        { subroutineBody = SubroutineBody localVarDecs statements
        , parameters = ParameterList parameters
        , ..
        } =
        let localTable = makeLocalTable classSymbolTable parameters localVarDecs
            initialization = case subroutineType of
                Constructor ->
                    -- Allocate memory & change THIS to returned address
                    [ push Constant classSymbolTable.fieldCounter
                    , call "Memory.alloc" 1
                    , pop Pointer 0
                    ]
                Method ->
                    -- Change THIS to the first argument
                    [ push Argument 0
                    , pop Pointer 0
                    ]
                Function ->
                    -- Functions do nothing special
                    []
         in [ functionDeclaration subroutineName localTable.localCounter
            ]
                <> initialization
                <> fst (generateStatements localTable statements)
      where
        functionDeclaration :: Text -> Word16 -> VM.Command
        functionDeclaration (unpack -> funcName) argCount =
            VM.FunctionCommand $ VM.Function (unpack classSymbolTable.className <> "." <> funcName) argCount
        makeLocalTable :: SymbolTable -> [(Type, Text)] -> [LocalVarDec] -> SymbolTable
        makeLocalTable symTable initialParams =
            let params =
                    if subroutineType == Method
                        then (ClassTy symTable.className, "this") : initialParams
                        else initialParams
             in L.foldl'
                    insertLocalSymbol
                    (L.foldl' insertParameterSymbol symTable params)
          where
            insertParameterSymbol :: SymbolTable -> (Type, Text) -> SymbolTable
            insertParameterSymbol st (varType, varName) = insertLocalVar varName varType LocalArg st
            insertLocalSymbol :: SymbolTable -> LocalVarDec -> SymbolTable
            insertLocalSymbol st (LocalVarDec varType names) =
                L.foldl' (\st' name -> insertLocalVar name varType LocalVar st') st names


generateStatements :: SymbolTable -> [Statement] -> ([VM.Command], SymbolTable)
generateStatements symTable =
    L.foldl'
        ( \(cmds, st) stmt ->
            let (newCmds, newTable) = generateStatement st stmt
             in (cmds <> newCmds, newTable)
        )
        ([], symTable)


generateStatement :: SymbolTable -> Statement -> ([VM.Command], SymbolTable)
generateStatement symTable = \case
    l@(LetStatement varName mbArrIndexExp expr) ->
        case getSymbol varName symTable of
            Nothing ->
                error $ "generateStatement: Left side of 'let' statement contains class name: " <> show l
            Just (_, ix, memorySegment) ->
                case mbArrIndexExp of
                    Nothing ->
                        ( generateExpression symTable expr
                            <> [pop memorySegment ix]
                        , symTable
                        )
                    Just arrayIxExpr ->
                        ( concat
                            [ [push memorySegment ix]
                            , generateExpression symTable arrayIxExpr
                            , [VM.ArithLogicCommand VM.Add]
                            , generateExpression symTable expr
                            , [pop Temp 0]
                            , [pop Pointer 1]
                            , [push Temp 0]
                            , [pop That 0]
                            ]
                        , symTable
                        )
    DoStatement subroutineCall ->
        ( generateSubroutineCall symTable subroutineCall
            <> [pop Temp 0]
        , symTable
        )
    ReturnStatement mbExpr ->
        ( maybe [push Constant 0] (generateExpression symTable) mbExpr
            <> [VM.FunctionCommand VM.Return]
        , symTable
        )
    WhileStatement conditionExpr statements ->
        let (loopSymTable, show -> loopLabel) = getNextLabel symTable
            (endLabelSymTable, show -> endLabel) = getNextLabel loopSymTable
            (statementCommands, finalSymTable) = generateStatements endLabelSymTable statements
         in ( concat
                [ [VM.BranchCommand $ VM.Label loopLabel]
                , generateExpression finalSymTable conditionExpr
                ,
                    [ VM.ArithLogicCommand VM.Not
                    , VM.BranchCommand $ VM.IfGoto endLabel
                    ]
                , statementCommands
                ,
                    [ VM.BranchCommand $ VM.Goto loopLabel
                    , VM.BranchCommand $ VM.Label endLabel
                    ]
                ]
            , finalSymTable
            )
    IfStatement conditionalExpr trueStatements falseStatements ->
        let (elseSymTable, show -> elseLabel) = getNextLabel symTable
            (endSymTable, show -> endLabel) = getNextLabel elseSymTable
            (trueCommands, trueSymTable) = generateStatements endSymTable trueStatements
            (falseCommands, falseSymTable) = generateStatements trueSymTable falseStatements
         in ( concat
                [ generateExpression falseSymTable conditionalExpr
                ,
                    [ VM.ArithLogicCommand VM.Not
                    , VM.BranchCommand $ VM.IfGoto elseLabel
                    ]
                , trueCommands
                ,
                    [ VM.BranchCommand $ VM.Goto endLabel
                    , VM.BranchCommand $ VM.Label elseLabel
                    ]
                , falseCommands
                , [VM.BranchCommand $ VM.Label endLabel]
                ]
            , falseSymTable
            )


generateExpression :: SymbolTable -> Expression -> [VM.Command]
generateExpression symTable = \case
    Expression term binOps ->
        generateTerm symTable term
            <> concatMap (\(op, otherTerm) -> generateTerm symTable otherTerm <> generateBinOp op) binOps


generateBinOp :: BinaryOp -> [VM.Command]
generateBinOp = \case
    Add -> [VM.ArithLogicCommand VM.Add]
    Minus -> [VM.ArithLogicCommand VM.Sub]
    Multiply -> [call "Math.multiply" 2]
    Divide -> [call "Math.divide" 2]
    And -> [VM.ArithLogicCommand VM.And]
    Or -> [VM.ArithLogicCommand VM.Or]
    LessThan -> [VM.ArithLogicCommand VM.Less]
    GreaterThan -> [VM.ArithLogicCommand VM.Greater]
    Equals -> [VM.ArithLogicCommand VM.Equals]


generateTerm :: SymbolTable -> Term -> [VM.Command]
generateTerm symTable = \case
    ParenthesizedTerm expr ->
        generateExpression symTable expr
    IntTerm i ->
        [ push Constant (fromIntegral i)
        ]
    KeywordTerm c ->
        case c of
            TrueC ->
                [ push Constant 1
                , VM.ArithLogicCommand VM.Neg
                ]
            FalseC ->
                [ push Constant 0
                ]
            NullC ->
                [ push Constant 0
                ]
            ThisC ->
                [ push Pointer 0
                ]
    StringTerm (unpack -> str) ->
        [ push Constant (fromIntegral $ length str)
        , call "String.new" 1
        ]
            <> concatMap
                ( \c ->
                    [ push Constant (fromIntegral $ ord c)
                    , call "String.appendChar" 2
                    ]
                )
                str
    t@(VarTerm varName) -> case getSymbol varName symTable of
        Nothing ->
            error $ "generateTerm: Variable Term not found in symbol table: " <> show t
        Just (_, ix, seg) ->
            [push seg ix]
    UnaryOpTerm unaryOp term ->
        generateTerm symTable term
            <> case unaryOp of
                Negate ->
                    [VM.ArithLogicCommand VM.Neg]
                Not ->
                    [VM.ArithLogicCommand VM.Not]
    SubroutineTerm subroutineCall ->
        generateSubroutineCall symTable subroutineCall
    t@(ArrayElementTerm varName ixExpr) -> case getSymbol varName symTable of
        Nothing ->
            error $ "generateTerm: Array Term not found in symbol table: " <> show t
        Just (_, ix, seg) ->
            concat
                [ [push seg ix]
                , generateExpression symTable ixExpr
                ,
                    [ VM.ArithLogicCommand VM.Add
                    , pop Pointer 1
                    , push That 0
                    ]
                ]


generateSubroutineCall :: SymbolTable -> SubroutineCall -> [VM.Command]
generateSubroutineCall symTable = \case
    ThisSubroutineCall methodName (ExpressionList exprs) ->
        concat
            [ [push Pointer 0]
            , concatMap (generateExpression symTable) exprs
            , [call (symTable.className <> "." <> methodName) (1 + fromIntegral (length exprs))]
            ]
    c@(ClassSubroutineCall varOrClassName funcName (ExpressionList exprs)) ->
        case getSymbol varOrClassName symTable of
            Nothing ->
                concatMap (generateExpression symTable) exprs
                    <> [call (varOrClassName <> "." <> funcName) (fromIntegral $ length exprs)]
            Just (ClassTy className, ix, seg) ->
                concat
                    [ [push seg ix]
                    , concatMap (generateExpression symTable) exprs
                    , [call (className <> "." <> funcName) (fromIntegral $ length exprs + 1)]
                    ]
            Just (ty, _, _) ->
                error $ "generateSubroutineCall: Tried calling function on invalid type '" <> show ty <> "': " <> show c


-- HELPERS

push :: VM.MemorySegment -> Word16 -> VM.Command
push seg = VM.StackCommand . VM.Push seg


pop :: VM.MemorySegment -> Word16 -> VM.Command
pop seg = VM.StackCommand . VM.Pop seg


call :: Text -> Word16 -> VM.Command
call funcName = VM.FunctionCommand . VM.Call (unpack funcName)
