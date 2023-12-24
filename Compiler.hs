{-# LANGUAGE LambdaCase #-}

module Compiler (main) where

import Compiler.Lexer (tok, tokenize)
import Compiler.TokenRenderer (tokensToXml)
import Control.Arrow ((&&&))
import Control.Monad (forM_)
import Data.List (intercalate)
import Data.Text.IO qualified as T (readFile, writeFile)
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
                    , "Reads input file or `.jack` files in directory."
                    , "Tokenizes each file exiting with error on failure."
                    , "Writes result to `<FILE_NAME>T.xml` in same directory as input file."
                    ]
            exitFailure


runCompiler :: FilePath -> IO ()
runCompiler jackPath = do
    files <- getFiles
    forM_ files $ \(outputPath, filePath) -> do
        txt <- T.readFile filePath
        xml <- case tokenize filePath txt of
            Left e -> print e >> exitFailure
            Right ts -> return . tokensToXml $ map tok ts
        T.writeFile outputPath xml
  where
    -- Get files to process & make the output file path
    getFiles :: IO [(FilePath, FilePath)]
    getFiles = do
        isFile <- doesFileExist jackPath
        if isFile
            then case splitExtension jackPath of
                (basePath, ".jack") ->
                    return [(basePath <> "T" <.> "xml", jackPath)]
                _ ->
                    error "ERROR: Expected input file with extension '.vm"
            else do
                allFiles <- map (jackPath </>) <$> listDirectory jackPath
                let jackFiles = filter ((== ".jack") . takeExtension) allFiles
                    mkOutputPath fileName = jackPath </> dropExtension (takeBaseName fileName) <> "T" <.> "xml"
                return $ map (mkOutputPath &&& id) jackFiles
