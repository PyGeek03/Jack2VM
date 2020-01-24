module Tokenizer
(tokenize
) where
import System.IO
import Data.List

splitWords = map words

removeComments :: [[String]] -> [[String]]
removeComments = 
    let findCommentStart singleLine = 
            case findIndex (\w -> w `elem` ["//", "/*", "/**", "*", "*/"]) singleLine of
                Just i  -> i
                Nothing -> length singleLine
        removeComment singleLine = take (findCommentStart singleLine) singleLine
    in map removeComment

tokenize :: String -> [String]
tokenize = mconcat . removeComments . splitWords . lines