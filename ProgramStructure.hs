module ProgramStructure where
import Data.List (unlines, intercalate)

import Tokens

-- Utilities
ops :: [Char]
ops = "+-*/&|<>="

unaryOps :: [Char]
unaryOps = ['-','~']

showsLines :: Show a => [a] -> String -> String
showsLines linesList = (++) (unlines $ map show linesList)

-- Data types for main program constructs
data Class 
    = Class { clsName :: !String
            , clsVars :: ![ClassVarDec]
            , clsSubs :: ![SubDec] }
instance Show Class where
    show c = "<class>\n\
             \<keyword> class </keyword>\n\
             \<identifier> " ++ clsName c ++ " </identifier>\n\
             \<symbol> { </symbol>\n"
             ++ (showsLines (clsVars c)
             . showsLines (clsSubs c))
             "<symbol> } </symbol>\n\
             \</class>"

newtype ClassVarDec = ClassVarDec [Token]
instance Show ClassVarDec where
    show (ClassVarDec vars) = "<classVarDec>\n"
                              ++ showsLines vars
                              "</classVarDec>"

data SubDec
    = SubDec { subKind   :: !String
             , subType   :: !Token
             , subClass  :: !String
             , subName   :: !String
             , subParams :: ![Token]
             , subBody   :: !SubBody }
instance Show SubDec where
    show sd = "<subroutineDec>\n\
              \<keyword> " ++ subKind sd ++ " </keyword>\n"
              ++ shows (subType sd) "\n\
              \<identifier> " ++ subName sd ++ " </identifier>\n\
              \<symbol> ( </symbol>\n\
              \<parameterList>\n"
              ++ showsLines (subParams sd)
              "</parameterList>\n\
              \<symbol> ) </symbol>\n"
              ++ shows (subBody sd) "\n\
              \</subroutineDec>"

data SubBody 
    = SubBody { subVars       :: ![VarDec]
              , subStatements :: !Statements }
instance Show SubBody where
    show sb = "<subroutineBody>\n\
              \<symbol> { </symbol>\n"
              ++ (showsLines (subVars sb)
              . shows (subStatements sb)) "\n\
              \<symbol> } </symbol>\n\
              \</subroutineBody>"

newtype VarDec = VarDec [Token]
instance Show VarDec where
    show (VarDec vars) = "<varDec>\n"
                         ++ showsLines vars
                         "</varDec>"

-- Data types for statements
newtype Statements = Statements [Statement]
instance Show Statements where
    show (Statements stms) = "<statements>\n"
                             ++ showsLines stms
                             "</statements>"

data Statement
    = LetArr { letArr        :: !String
             , letArrMapping :: !String
             , letArrIndex   :: !Expression
             , letArrValue   :: !Expression }
    | LetVar { letVar        :: !String
             , letVarMapping :: !String
             , letVarValue   :: !Expression }
    | If { ifCond     :: !Expression
         , thenClause :: !Statements
         , elseClause :: Maybe Statements
         , ifLabel    :: !String }
    | While { whileCond  :: !Expression
            , whileDo    :: !Statements
            , whileLabel :: !String }
    | Do { doCls  :: !String
         , hasCls :: !Bool
         , doSub  :: !String
         , doArgs :: !ExpressionList }
    | Return { returnValue :: Maybe Expression }
instance Show Statement where
    show (LetArr arr _ i value) = "<letStatement>\n\
                                  \<keyword> let </keyword>\n\
                                  \<identifier> "++ arr ++ " </identifier>\n\
                                  \<symbol> [ </symbol>\n"
                                  ++ shows i "\n\
                                  \<symbol> ] </symbol>\n\
                                  \<symbol> = </symbol>\n"
                                  ++ shows value "\n\
                                  \<symbol> ; </symbol>\n\
                                  \</letStatement>"
    show (LetVar var _ value) = "<letStatement>\n\
                                 \<keyword> let </keyword>\n\
                                 \<identifier> " ++ var ++ " </identifier>\n\
                                 \<symbol> = </symbol>\n"
                                 ++ shows value "\n\
                                 \<symbol> ; </symbol>\n\
                                 \</letStatement>"
    show (If cond thenDo elseDo _) = "<ifStatement>\n\
                                     \<keyword> if </keyword>\n\
                                     \<symbol> ( </symbol>\n"
                                     ++ shows cond "\n\
                                     \<symbol> ) </symbol>\n\
                                     \<symbol> { </symbol>\n"
                                     ++ shows thenDo "\n\
                                     \<symbol> } </symbol>\n"
                                     ++ showsElse elseDo
                                     "</ifStatement>"
      where
        showsElse (Just s) = (++) ("<keyword> else </keyword>\n\
                                   \<symbol> { </symbol>\n"
                                   ++ shows s "\n\
                                   \<symbol> } </symbol>\n")
        showsElse Nothing = (++) ""
    show (While cond loop _) = "<whileStatement>\n\
                               \<keyword> while </keyword>\n\
                               \<symbol> ( </symbol>\n"
                               ++ shows cond "\n\
                               \<symbol> ) </symbol>\n\
                               \<symbol> { </symbol>\n"
                               ++ shows loop "\n\
                               \<symbol> } </symbol>\n\
                               \</whileStatement>"
    show (Do className explicit subroutineName exprList) = 
        let showsClsName = if explicit
            then (++) ("<identifier> " ++ className ++" </identifier>\n\
                       \<symbol> . </symbol>\n")
            else (++) ""
        in "<doStatement>\n\
           \<keyword> do </keyword>\n"
           ++ showsClsName
           "<identifier> " ++ subroutineName ++ " </identifier>\n\
           \<symbol> ( </symbol>\n"
           ++ shows exprList "\n\
           \<symbol> ) </symbol>\n\
           \<symbol> ; </symbol>\n\
           \</doStatement>"
    show (Return mexp) = "<returnStatement>\n\
                         \<keyword> return </keyword>\n"
                         ++ showsMaybeExp mexp
                         "<symbol> ; </symbol>\n\
                         \</returnStatement>"
      where
        showsMaybeExp (Just e) = (++) (shows e "\n")
        showsMaybeExp Nothing  = (++) ""

-- Data types for expressions
newtype ExpressionList = ExpressionList [Expression]
instance Show ExpressionList where
    show (ExpressionList []) = "<expressionList>\n\
                               \</expressionList>"
    show (ExpressionList exprs) = 
        "<expressionList>\n"
        ++ (intercalate "\n<symbol> , </symbol>\n" $ map show exprs)
        ++ "\n</expressionList>"

data Expression = Expression !Term (Maybe (Token, Term))
instance Show Expression where
    show (Expression root mFollow) = "<expression>\n"
                                   ++ shows root "\n"
                                   ++ showsFollow mFollow
                                   "</expression>"
      where
        showsFollow (Just (op, term)) = (++) (shows op "\n" ++ shows term "\n")
        showsFollow Nothing = (++) ""

data Term 
    = Call { callCls  :: !String
           , callSub  :: !String
           , callArgs :: !ExpressionList }
    | Arr { arrToGet   :: !String
          , arrMapping :: !String
          , arrIndex   :: !Expression }
    | Var { varToGet   :: !String
          , varMapping :: !String }
    | Parens !Expression
    | Unary { unaryOp   :: !Token
            , unaryTerm :: !Term }
    | Const !Token
instance Show Term where
    show (Call c s as) = "<term>\n\
                         \<identifier> " ++ c ++ " </identifier>\n\
                         \<symbol> . </symbol>\n\
                         \<identifier> " ++ s ++ " </identifier>\n\
                         \<symbol> ( </symbol>\n"
                         ++ shows as "\n\
                         \<symbol> ) </symbol>\n\
                         \</term>"
    show (Arr a _ i) = "<term>\n\
                       \<identifier> " ++ a ++ " </identifier>\n\
                       \<symbol> [ </symbol>\n"
                       ++ shows i "\n\
                       \<symbol> ] </symbol>\n\
                       \</term>"
    show (Var v _) = "<term>\n\
                     \<identifier> " ++ v ++ " </identifier>\n\
                     \</term>"
    show (Parens e) = "<term>\n\
                      \<symbol> ( </symbol>\n"
                      ++ shows e "\n\
                      \<symbol> ) </symbol>\n\
                      \</term>"
    show (Unary op t) = "<term>\n"
                        ++ shows op "\n"
                        ++ shows t "\n\
                        \</term>"
    show (Const c) = "<term>\n"
                     ++ shows c "\n\
                     \</term>"