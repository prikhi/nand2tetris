{-# LANGUAGE LambdaCase #-}

module Assembler where

import Assembler.CodeGen (generateInstructions)
import Assembler.Parser (runParser)
import Data.List (intercalate)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (readFile')


-- | Valiate args & run
main :: IO ()
main =
    getArgs >>= \case
        [asmFile] ->
            runAssembler asmFile
        args -> do
            putStrLn $
                unlines
                    [ "ERROR: Expected 1 argument, assembly file name, got: "
                    , ""
                    , "\t" <> intercalate ", " (map show args)
                    , ""
                    , "Assembler.hs <FILE_NAME>.asm"
                    , ""
                    , "Reads input file."
                    , "Translates to Hack machine code."
                    , "Write results to <FILE_NAME>.hack in same directory as input file."
                    ]
            exitFailure


-- | Get input file text, parse it, generate the machine code, & write to
-- output file.
runAssembler :: FilePath -> IO ()
runAssembler asmPath = do
    inputText <- readFile' asmPath
    let parsedAssembly = runParser inputText
    let binaryInstructions = generateInstructions parsedAssembly
    let outputPath = stripExtension asmPath <> ".hack"
    writeFile outputPath $ unlines binaryInstructions
  where
    stripExtension :: FilePath -> FilePath
    stripExtension fp = case reverse fp of
        'm' : 's' : 'a' : '.' : rest ->
            reverse rest
        _ -> error "ERROR: Expected input file with extension '.asm'"
