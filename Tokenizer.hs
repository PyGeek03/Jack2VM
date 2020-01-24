module Tokenizer
(tokenize
, Token
) where
--import System.IO
import Data.List
import Text.Read (readMaybe)
import Data.Maybe (isJust, fromJust)

-- Data type for each token
data Token = Keyword String | Symbol Char | IntConst Int | StrConst String | Identifier String

showToken :: Token -> String
showToken (Keyword k) = "<keyword> " ++ k ++ " </keyword>"
showToken (Symbol s) = case s of
    '<' -> "<symbol> &lt; </symbol>"
    '>' -> "<symbol> &gt; </symbol>"
    '&' -> "<symbol> &amp; </symbol>"
    _   -> "<symbol> " ++ [s] ++ " </symbol>"
showToken (IntConst ic) = "<integerConstant> " ++ show ic ++ " </integerConstant>"
showToken (StrConst sc) = "<stringConstant> " ++ sc ++ " </stringConstant>"
showToken (Identifier i) = "<identifier> " ++ i ++ " </identifier>"

instance Show Token where
    show = showToken

-- Data type for all tokens in a Jack file
data Tokens = Tokens [Token]

showTokens :: Tokens -> String
showTokens (Tokens ts) = "<tokens>\n" ++ (unlines $ map show ts) ++ "</tokens>\n"

instance Show Tokens where
    show = showTokens

-- Lexical elements
keywords =  [ "class", "constructor", "function", "method", "field"
            , "static", "var", "int", "char", "boolean", "void"
            , "true", "false", "null", "this", "let", "do", "if"
            , "else", "while", "return" ]
symbols = "{}()[].,;+-*/&|<>=~"
whitespace = " \r\n\t"

readInt :: String -> Maybe Int
readInt token = readMaybe token :: Maybe Int

classify :: String -> Token
classify token = case readInt token of
    Just i -> IntConst i
    Nothing
        | "\"" `isPrefixOf` token -> StrConst $ init $ tail token
        | token `elem` keywords   -> Keyword token
        | otherwise               -> Identifier token    

makeTokens :: String -> String -> [Token] -> Tokens
makeTokens acc source reversedTokenList = case (acc, source) of
    -- Base case: source is empty -> return
    (_, [])        -> Tokens $ reverse reversedTokenList
    -- Deal with '/'
    ("/", x:xs) ->
        case x of
            '/' -> makeTokens "//" xs reversedTokenList  -- starts new comment
            '*' -> makeTokens "/*" xs reversedTokenList  -- starts new multiline comment
            _   -> makeTokens "" xs $ (Symbol '/'):reversedTokenList  -- new '/' symbol
    ("//", x:xs)
        | x `elem` "\r\n" -> makeTokens "" xs reversedTokenList  -- end of line -> end of comment
        | otherwise       -> makeTokens "//" xs reversedTokenList  -- still in comment
    ("/*", x:y:xs) ->
        case [x, y] of
            "*/" -> makeTokens "" xs reversedTokenList  -- end of multiline comment
            _    -> makeTokens "/*" (y:xs) reversedTokenList  -- still in multiline comment
    -- Build actual tokens
    ('"':as, x:xs) ->
        case x of
            '"' -> makeTokens "" xs $ (StrConst $ tail acc):reversedTokenList  -- completed stringConst
            _   -> makeTokens (acc ++ [x]) xs reversedTokenList  -- still in stringConst
    ("", x:xs)
        | x `elem` whitespace -> makeTokens "" xs reversedTokenList  -- skip whitespace
        | x == '/'            -> makeTokens "/" xs reversedTokenList  -- '/' could either be a standalone symbol or the beginning of a comment
        | x `elem` symbols    -> makeTokens "" xs $ (Symbol x):reversedTokenList  -- new symbol
        | otherwise           -> makeTokens [x] xs reversedTokenList  -- starts new token
    (_, x:xs)
        | x `elem` whitespace -> makeTokens "" xs $ (classify acc):reversedTokenList  -- completed token
        | x == '/'            -> makeTokens "/" xs $ (classify acc):reversedTokenList  -- '/' could either be a standalone symbol or the beginning of a comment
        | x `elem` symbols    -> makeTokens "" xs $ (Symbol x):(classify acc):reversedTokenList  -- completed token & new symbol
        | otherwise           -> makeTokens (acc ++ [x]) xs reversedTokenList  -- continues current token

tokenize :: String -> Tokens
tokenize source = makeTokens "" source []