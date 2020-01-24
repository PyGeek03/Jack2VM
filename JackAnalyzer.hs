module Main where
import System.Environment
import Data.List (isSuffixOf)
import System.FilePath (dropExtension, (</>))
import System.Directory (getDirectoryContents)
import Tokenizer (tokenize, Token)

compileJack :: FilePath -> IO ()
compileJack jackFile = do
    contents <- readFile jackFile
    let tXmlFile = (dropExtension jackFile) ++ "T.xml"
        xmlFile = (dropExtension jackFile) ++ ".xml"
        tokens = tokenize contents
    writeFile tXmlFile $ show tokens

compileDir :: FilePath -> IO [()]
compileDir dirPath = do
    allFilenames <- getDirectoryContents dirPath
    let allFiles = map (dirPath </>) allFilenames
        jackFiles = filter (\x -> ".jack" `isSuffixOf` x) allFiles
    mapM compileJack jackFiles

main :: IO [()]
main = do
    (inputPath:[]) <- getArgs
    if (dropExtension inputPath) == inputPath  -- input is a directory
        then compileDir inputPath
        else mapM compileJack [inputPath]