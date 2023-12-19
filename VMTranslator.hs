{-# LANGUAGE LambdaCase #-}

module VMTranslator (main) where

import Assembler.Render (renderAssemblyLine)
import Data.List (intercalate)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (readFile')
import VMTranslator.CodeGen (generateAssembly)
import VMTranslator.Parser (runParser)


-- | Valiate args & run
main :: IO ()
main =
    getArgs >>= \case
        [vmFile] ->
            runTranslator vmFile
        args -> do
            putStrLn $
                unlines
                    [ "ERROR: Expected 1 argument, VM file name, got: "
                    , ""
                    , "\t" <> intercalate ", " (map show args)
                    , ""
                    , "VMTranslator.hs <FILE_NAME>.vm"
                    , ""
                    , "Reads input file."
                    , "Translates to Hack Hack assembly code."
                    , "Write results to <FILE_NAME>.asm in same directory as input file."
                    ]
            exitFailure


-- | Get input file text, parse it, generate the machine code, & write to
-- output file.
runTranslator :: FilePath -> IO ()
runTranslator vmPath = do
    inputText <- readFile' vmPath
    let parsedCommands = runParser inputText
    let generatedAssembly = generateAssembly parsedCommands
    let outputPath = stripExtension vmPath <> ".asm"
    writeFile outputPath $ unlines $ map renderAssemblyLine generatedAssembly
  where
    stripExtension :: FilePath -> FilePath
    stripExtension fp = case reverse fp of
        'm' : 'v' : '.' : rest ->
            reverse rest
        _ -> error "ERROR: Expected input file with extension '.vm"
