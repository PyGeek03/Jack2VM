module Compiler where
import Data.Maybe (fromMaybe)
import Data.List  (intercalate)
import Data.Char  (ord)

import ProgramStructure hiding (showsLines)
import Tokens


toString :: [String] -> String
toString = intercalate "\n"

class Compile c where
    compile :: c -> String

instance Compile Class where
    compile = toString . map compile . clsSubs

instance Compile SubDec where
    compile sub =
        let func   = subClass sub ++ "." ++ subName sub
            nVars  = varCount $ subBody sub
            header = case subKind sub of
                "constructor" -> 
                    toString [ ""
                             , "push constant " ++ show (clsFields sub)
                             , "call Memory.alloc 1"
                             , "pop pointer 0" ]
                "method" -> "\npush argument 0\n\
                            \pop pointer 0"
                "function" -> ""
                _ -> error "Error: Invalid subroutine kind"
        in toString [ "function " ++ func ++ " " ++ show nVars
                    ++ header
                    , compile $ subBody sub ]

instance Compile SubBody where
    compile = compile . subStatements

instance Compile Statements where
    compile (Statements stms) = toString $ map compile stms

instance Compile Statement where
    compile (LetArr arr push index value) = toString [ push ++ "  // " ++ arr
                                                     , compile index
                                                     , "add"
                                                     , compile value
                                                     , "pop temp 0"
                                                     , "pop pointer 1"
                                                     , "push temp 0"
                                                     , "pop that 0"
                                                     ]
    compile (LetVar var pop value) = compile value ++ "\n"
                                     ++ pop ++ "  // " ++ var
    compile (If cond thenDo mElseDo labelID) =
        let true  = "IF_TRUE" ++ labelID
            false = "IF_FALSE" ++ labelID
            end   = "IF_END" ++ labelID
        in toString [ compile cond
                    , "if-goto " ++ true
                    , "goto "    ++ false
                    , "label " ++ true
                    , compile thenDo
                    , "goto "  ++ end
                    , "label " ++ false
                    , fromMaybe "" $ fmap compile mElseDo
                    , "label " ++ end
                    ]
    compile (While cond loop labelID) =
        let while = "WHILE_EXP" ++ labelID
            end   = "WHILE_END" ++ labelID
        in toString [ "label " ++ while
                    , compile cond
                    , "not"
                    , "if-goto " ++ end
                    , compile loop
                    , "goto "  ++ while
                    , "label " ++ end
                    ]
    compile (Do subCall) = compile subCall ++ "\n\
                          \pop temp 0"
    compile (Return mValue) = case mValue of
        Nothing    -> "push constant 0\n\
                      \return"
        Just value -> compile value ++ "\n\
                      \return"

instance Compile SubCall where
    compile (SubCall _ mPush _ cls sub (ExpressionList exprs)) =
        let thisForMethod = case mPush of
                Nothing   -> ""
                Just this -> this ++ "\n"
            compiledExprs  = toString (map compile exprs)
            args = case compiledExprs of
                ""        -> ""
                arguments -> arguments ++ "\n"
            func  = cls ++ "." ++ sub
            nArgs = length exprs + (fromEnum . not . null $ thisForMethod)
        in thisForMethod
           ++ args
           ++ "call " ++ func ++ " " ++ show nArgs

instance Compile Expression where
    compile (Expression term1 mFollow) = case mFollow of
        Nothing          -> compile term1
        Just (op, term2) -> toString [ compile term1
                                     , compile term2
                                     , compile op ]

instance Compile Term where
    compile (Arr arr push index) = toString [ push ++ "  // " ++ arr
                                            , compile index
                                            , "add"
                                            , "pop pointer 1"
                                            , "push that 0" ]
    compile (Var var push) = push ++ "  // " ++ var
    compile (Call subCall) = compile subCall
    compile (Parens expr)  = compile expr
    compile (Const token)  = compile token
    compile (Unary (SYM '-') term) = compile term ++ "\n\
                                     \neg"
    compile (Unary (SYM '~') term) = compile term ++ "\n\
                                     \not"
    compile (Unary _ _)            = error "Invalid unary op"

instance Compile Token where
    compile (KW "this") = "push pointer 0"
    compile (KW "null") = "push constant 0"
    compile (KW "true") = "push constant 0\n\
                          \not"
    compile (KW "false") = "push constant 0"
    compile (IC  num) = "push constant " ++ show num
    compile (SYM '+') = "add"
    compile (SYM '-') = "sub"
    compile (SYM '*') = "call Math.multiply 2"
    compile (SYM '/') = "call Math.divide 2"
    compile (SYM '=') = "eq"
    compile (SYM '>') = "gt"
    compile (SYM '<') = "lt"
    compile (SYM '&') = "and"
    compile (SYM '|') = "or"
    compile (SYM '~') = "not"
    compile (SC string) =
        toString [ "push constant " ++ show (length string)
                 , "call String.new 1"
                 , toString $ map (\c -> "push constant " 
                                         ++ shows (ord c) " // " ++ shows c "\n\
                                         \call String.appendChar 2") string
                 ]
    compile _ = error "Error: Invalid token"