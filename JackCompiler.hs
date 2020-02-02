module Main where
import System.Environment (getArgs)
import Data.List (isSuffixOf)
import Data.Foldable (traverse_)
import System.FilePath (dropExtension, (</>))
import System.Directory (getDirectoryContents)

import Tokenizer (tokenize)
import Parser (parse)
import Compiler

compileJack :: FilePath -> IO ()
compileJack jackFile = do
    contents <- readFile jackFile
    let vmFile = (dropExtension jackFile) ++ ".vm"
        tokens = tokenize contents
        ast = parse tokens
    writeFile vmFile $ compile ast

compileDir :: FilePath -> IO ()
compileDir dirPath = do
    allFilenames <- getDirectoryContents dirPath
    let allFiles = map (dirPath </>) allFilenames
        jackFiles = filter (\x -> ".jack" `isSuffixOf` x) allFiles
    traverse_ compileJack jackFiles

main :: IO ()
main = do
    (inputPath:[]) <- getArgs
    if (dropExtension inputPath) == inputPath  -- input is a directory
        then compileDir inputPath
        else compileJack inputPath