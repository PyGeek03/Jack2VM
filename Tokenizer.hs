module Tokenizer
(tokenize) where
import Data.List (isPrefixOf)
import Text.Read (readMaybe)

import Tokens


whitespace :: [Char]
whitespace = " \r\n\t"


classify :: String -> Token
classify token = case readInt token of
    Just i -> IC i
    Nothing
        | "\"" `isPrefixOf` token -> SC $ init $ tail token
        | token `elem` keywords   -> KW token
        | otherwise               -> ID token
  where
    readInt t = readMaybe t :: Maybe Int
    keywords =  [ "class", "constructor", "function", "method", "field"
                , "static", "var", "int", "char", "boolean", "void"
                , "true", "false", "null", "this", "let", "do", "if"
                , "else", "while", "return" ]

makeTokens :: Int -> String -> [Token] -> String -> Tokens
makeTokens count acc reversedTokenList source = case (acc, source) of
    -- Base case: source is empty -> return
    (_,       []) -> Tokens $ reverse reversedTokenList
    -- '/' is either a standalone symbol or the beginning of a comment
    ("",  '/':xs) -> makeTokens count "/"  reversedTokenList xs
    ("/", '*':xs) -> makeTokens count "/*" reversedTokenList xs  -- starts new multiline comment
    ("/", '/':xs) -> makeTokens count "//" reversedTokenList xs  -- starts new comment
    ("/",     xs) -> makeTokens count ""   (SYM '/' : reversedTokenList) xs  -- new '/' symbol
    -- Deal with multiline comments
    ("/*", '*':'/':xs) -> makeTokens count ""   reversedTokenList xs  -- end of multiline comment
    ("/*",  _ : y :xs) -> makeTokens count "/*" reversedTokenList $ y:xs -- still in multiline comment
    ("//", x:xs)
        | x `elem` "\r\n" -> makeTokens count ""   reversedTokenList xs  -- end of line -> end of comment
        | otherwise       -> makeTokens count "//" reversedTokenList xs  -- still in comment
    -- Build string constants
    ('"':_, '"':xs) -> makeTokens count "" ((SC $ tail acc):reversedTokenList) xs  -- completed stringConst
    ('"':_,  x :xs) -> makeTokens count (acc ++ [x]) reversedTokenList xs  -- still in stringConst
    -- Build other tokens
    ("", x:xs)
        | x `elem` whitespace -> makeTokens count ""  reversedTokenList xs  -- skip whitespace
        | x `elem` symbols    -> makeTokens count ""  (SYM x:reversedTokenList) xs  -- new symbol
        | otherwise           -> makeTokens count [x] reversedTokenList xs  -- starts new token
    ("if",    xs) -> makeTokens (count+1) "" (IF    count:reversedTokenList) xs
    ("while", xs) -> makeTokens (count+1) "" (WHILE count:reversedTokenList) xs
    (_, '/':xs) -> makeTokens count "/"  (classify acc : reversedTokenList) xs
    (_,   x:xs)
        | x `elem` whitespace -> makeTokens count "" (classify acc : reversedTokenList) xs  -- completed token
        | x `elem` symbols    -> makeTokens count "" (SYM x : classify acc : reversedTokenList) xs  -- completed token & new symbol
        | otherwise           -> makeTokens count (acc ++ [x]) reversedTokenList xs -- continues current token
  where
    symbols = "{}()[].,;+-*/&|<>=~"

tokenize :: String -> Tokens
tokenize = makeTokens 0 "" []
