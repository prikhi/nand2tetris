{-# LANGUAGE LambdaCase #-}

module Compiler (main) where

import Compiler.CodeGen (generateClass)
import Compiler.Lexer (tokenize)
import Compiler.Parser (parseAST)
import Control.Arrow ((&&&))
import Control.Monad (forM_, when)
import Data.List (intercalate)
import Data.Text.IO qualified as T (readFile)
import Data.Text.Lazy qualified as TL (pack, unlines)
import Data.Text.Lazy.IO qualified as TL (writeFile)
import System.Directory (doesFileExist, getCurrentDirectory, listDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath
    ( dropExtension
    , splitExtension
    , takeExtension
    , (<.>)
    , (</>)
    )
import VMTranslator.Render (renderVMCommand)


main :: IO ()
main =
    getArgs >>= \case
        [jackFileOrDir] ->
            runCompiler jackFileOrDir
        [] ->
            getCurrentDirectory >>= runCompiler
        args -> do
            putStrLn $
                unlines
                    [ "ERROR: Expected 0 or 1 arguments, got: "
                    , ""
                    , "\t" <> intercalate ", " (map show args)
                    , ""
                    , "Compiler.hs [<FILE_NAME>.jack|<DIRECTORY>]"
                    , ""
                    , "Reads input file or `.jack` files in input directory."
                    , "Tokenizes, parses, & compiles each file into VM commands."
                    , "Writes results to `<FILE_NAME>.vm` in same directory as input file."
                    , "Exits with error on failure."
                    ]
            exitFailure


runCompiler :: FilePath -> IO ()
runCompiler jackPath = do
    files <- getFiles
    forM_ files $ \(outputPath, filePath) -> do
        txt <- T.readFile filePath
        case tokenize filePath txt >>= parseAST filePath of
            Left e -> print e >> exitFailure
            Right (jackClass, _parseTreeXml) -> do
                let commands = TL.pack . renderVMCommand <$> generateClass jackClass
                TL.writeFile outputPath (TL.unlines commands)
  where
    -- Get files to process & make the output file paths
    getFiles :: IO [(FilePath, FilePath)]
    getFiles = do
        isFile <- doesFileExist jackPath
        if isFile
            then case splitExtension jackPath of
                (basePath, ".jack") ->
                    return [(basePath <.> "vm", jackPath)]
                _ ->
                    error "ERROR: Expected input file with extension '.jack"
            else do
                allFiles <- map (jackPath </>) <$> listDirectory jackPath
                let jackFiles = filter ((== ".jack") . takeExtension) allFiles
                when (null jackFiles) $
                    error "ERROR: No '.jack' files found in directory."
                let mkOutputPath fileName = dropExtension fileName <.> "vm"
                return $ map (mkOutputPath &&& id) jackFiles
