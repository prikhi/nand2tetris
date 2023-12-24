{-# LANGUAGE LambdaCase #-}

module Compiler (main) where

import Compiler.Lexer (tokenize)
import Compiler.Parser (parseAST)
import Control.Arrow ((&&&))
import Control.Monad (forM_, when)
import Data.List (intercalate)
import Data.Text.IO qualified as T (readFile)
import Data.Text.Lazy.IO qualified as TL (writeFile)
import System.Directory (doesFileExist, getCurrentDirectory, listDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath
    ( dropExtension
    , splitExtension
    , takeBaseName
    , takeExtension
    , (<.>)
    , (</>)
    )


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
                    , "Tokenizes & parses each file, exiting with error on failure."
                    , "Writes result to `<FILE_NAME>.xml` in same directory as input file."
                    ]
            exitFailure


runCompiler :: FilePath -> IO ()
runCompiler jackPath = do
    files <- getFiles
    forM_ files $ \(outputPath, filePath) -> do
        txt <- T.readFile filePath
        case tokenize filePath txt >>= parseAST filePath of
            Left e -> print e >> exitFailure
            Right (_, xml) -> TL.writeFile outputPath xml
  where
    -- Get files to process & make the output file paths
    getFiles :: IO [(FilePath, FilePath)]
    getFiles = do
        isFile <- doesFileExist jackPath
        if isFile
            then case splitExtension jackPath of
                (basePath, ".jack") ->
                    return [(basePath <.> "xml", jackPath)]
                _ ->
                    error "ERROR: Expected input file with extension '.jack"
            else do
                allFiles <- map (jackPath </>) <$> listDirectory jackPath
                let jackFiles = filter ((== ".jack") . takeExtension) allFiles
                when (null jackFiles) $
                    error "ERROR: No '.jack' files found in directory."
                let mkOutputPath fileName = jackPath </> dropExtension (takeBaseName fileName) <.> "xml"
                return $ map (mkOutputPath &&& id) jackFiles
