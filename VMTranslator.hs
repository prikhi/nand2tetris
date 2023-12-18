{-# LANGUAGE LambdaCase #-}

module VMTranslator (main) where

import Data.List (intercalate)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (readFile')
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
    mapM_ print parsedCommands
  where
    -- let binaryInstructions = generateInstructions parsedAssembly
    -- let outputPath = stripExtension asmPath <> ".asm"
    -- writeFile outputPath $ unlines binaryInstructions

    stripExtension :: FilePath -> FilePath
    stripExtension fp = case reverse fp of
        'm' : 'v' : '.' : rest ->
            reverse rest
        _ -> error "ERROR: Expected input file with extension '.vm"
