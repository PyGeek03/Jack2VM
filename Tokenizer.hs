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

makeTokens :: String -> [Token] -> String -> Tokens
makeTokens acc reversedTokenList source = case (acc, source) of
    -- Base case: source is empty -> return
    (_,       []) -> Tokens $ reverse reversedTokenList
    -- '/' is either a standalone symbol or the beginning of a comment
    ("",  '/':xs) -> makeTokens "/"  reversedTokenList xs
    ("/", '*':xs) -> makeTokens "/*" reversedTokenList xs  -- starts new multiline comment
    ("/", '/':xs) -> makeTokens "//" reversedTokenList xs  -- starts new comment
    ("/",     xs) -> makeTokens ""   (SYM '/' : reversedTokenList) xs  -- new '/' symbol
    -- Deal with multiline comments
    ("/*", '*':'/':xs) -> makeTokens ""   reversedTokenList xs  -- end of multiline comment
    ("/*",  _ : y :xs) -> makeTokens "/*" reversedTokenList $ y:xs -- still in multiline comment
    ("//", x:xs)
        | x `elem` "\r\n" -> makeTokens ""   reversedTokenList xs  -- end of line -> end of comment
        | otherwise       -> makeTokens "//" reversedTokenList xs  -- still in comment
    -- Build string constants
    ('"':_, '"':xs) -> makeTokens "" ((SC $ tail acc):reversedTokenList) xs  -- completed stringConst
    ('"':_,  x :xs) -> makeTokens (acc ++ [x]) reversedTokenList xs  -- still in stringConst
    -- Build other tokens
    ("", x:xs)
        | x `elem` whitespace -> makeTokens ""  reversedTokenList xs  -- skip whitespace
        | x `elem` symbols    -> makeTokens ""  (SYM x:reversedTokenList) xs  -- new symbol
        | otherwise           -> makeTokens [x] reversedTokenList xs  -- starts new token
    (_, '/':xs) -> makeTokens "/"  (classify acc : reversedTokenList) xs
    (_,   x:xs)
        | x `elem` whitespace -> makeTokens "" (classify acc : reversedTokenList) xs  -- completed token
        | x `elem` symbols    -> makeTokens "" (SYM x : classify acc : reversedTokenList) xs  -- completed token & new symbol
        | otherwise           -> makeTokens (acc ++ [x]) reversedTokenList xs -- continues current token
  where
    symbols = "{}()[].,;+-*/&|<>=~"

tokenize :: String -> Tokens
tokenize = makeTokens "" []
