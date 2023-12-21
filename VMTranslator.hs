{-# LANGUAGE LambdaCase #-}

module VMTranslator (main) where

import Assembler.Render (renderAssemblyLine)
import Control.Monad (forM)
import Data.List (intercalate)
import System.Directory (doesFileExist, getCurrentDirectory, listDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath
    ( dropTrailingPathSeparator
    , splitExtension
    , takeBaseName
    , takeExtension
    , takeFileName
    , (<.>)
    , (</>)
    )
import System.IO (readFile')
import VMTranslator.CodeGen (generateAssembly)
import VMTranslator.Parser (runParser)


-- | Valiate args & run
main :: IO ()
main =
    getArgs >>= \case
        [vmFileOrDir] ->
            runTranslator vmFileOrDir
        [] ->
            getCurrentDirectory >>= runTranslator
        args -> do
            putStrLn $
                unlines
                    [ "ERROR: Expected 0 or 1 arguments, got: "
                    , ""
                    , "\t" <> intercalate ", " (map show args)
                    , ""
                    , "VMTranslator.hs [<FILE_NAME>.vm|<DIRECTORY>]"
                    , ""
                    , "Reads input file or `.vm` files in directory."
                    , "Translates to Hack assembly code."
                    , "Write results to <FILE_NAME>.asm in same directory as input file."
                    ]
            exitFailure


-- | Get input file text, parse it, generate the machine code, & write to
-- output file.
runTranslator :: FilePath -> IO ()
runTranslator vmPath = do
    (outputPath, files) <- getFiles
    parsedFiles <- forM files $ \file -> do
        inputText <- readFile' file
        let parsedCommands = runParser inputText
        return (takeBaseName file, parsedCommands)
    let generatedAssembly = generateAssembly parsedFiles
    writeFile outputPath $ unlines $ map renderAssemblyLine generatedAssembly
  where
    -- Get files to process & make the output file path
    getFiles :: IO (FilePath, [FilePath])
    getFiles = do
        isFile <- doesFileExist vmPath
        if isFile
            then case splitExtension vmPath of
                (basePath, ".vm") ->
                    return (basePath <.> "asm", [vmPath])
                _ ->
                    error "ERROR: Expected input file with extension '.vm"
            else do
                let folderName = takeFileName $ dropTrailingPathSeparator vmPath
                    outputPath = vmPath </> folderName <.> "asm"
                allFiles <- map (vmPath </>) <$> listDirectory vmPath
                let vmFiles = filter ((== ".vm") . takeExtension) allFiles
                return (outputPath, vmFiles)
