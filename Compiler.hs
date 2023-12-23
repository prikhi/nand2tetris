{-# LANGUAGE LambdaCase #-}

module Compiler (main) where

import Compiler.Lexer (tokenize)
import Data.List (intercalate)
import Data.Text.IO qualified as T (readFile)
import System.Environment (getArgs)
import System.Exit (exitFailure)


main :: IO ()
main =
    getArgs >>= \case
        [jackFileOrDir] ->
            runCompiler jackFileOrDir
        args -> do
            putStrLn $
                unlines
                    [ "ERROR: Expected 1 argument, got: "
                    , ""
                    , "\t" <> intercalate ", " (map show args)
                    , ""
                    , "Compiler.hs <FILE_NAME>.jack"
                    , ""
                    , "Reads input file into token stream."
                    , "Prints out result or error."
                    ]
            exitFailure


runCompiler :: FilePath -> IO ()
runCompiler jackPath = do
    txt <- T.readFile jackPath
    case tokenize jackPath txt of
        Left e -> print e
        Right ts -> mapM_ print ts
