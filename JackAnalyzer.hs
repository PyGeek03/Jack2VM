module Main where
import System.Environment
import Tokenizer (tokenize)

main :: IO ()
main = do
    (input:[]) <- getArgs
    contents <- readFile input
    -- (putStr . unlines . tokenize) contents
    (print . tokenize) contents
    -- print contents