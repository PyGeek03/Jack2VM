module Tokens where 
import Data.List (unlines)

-- Data type to "wrap" all tokens in one type
data Token 
    = KW  !String
    | SYM !Char
    | IC  !Int
    | SC  !String
    | ID  !String
    | IF    !Int
    | WHILE !Int
instance Show Token where
    show (KW k) = "<keyword> " ++ k ++ " </keyword>"
    show (SYM s) = case s of
        '<' -> "<symbol> &lt; </symbol>"
        '>' -> "<symbol> &gt; </symbol>"
        '&' -> "<symbol> &amp; </symbol>"
        _   -> "<symbol> " ++ [s] ++ " </symbol>"
    show (IC ic) = "<integerConstant> " ++ shows ic " </integerConstant>"
    show (SC sc) = "<stringConstant> " ++ sc ++ " </stringConstant>"
    show (ID i) = "<identifier> " ++ i ++ " </identifier>"
    show (IF i) = "<keyword ID = \"" ++ shows i "\"> if </keyword>"
    show (WHILE i) = "<keyword ID = \"" ++ shows i "\"> while </keyword>"

newtype Tokens = Tokens [Token]
instance Show Tokens where
    show (Tokens ts) = "<tokens>\n" ++ (unlines $ map show ts) ++ "</tokens>\n"
