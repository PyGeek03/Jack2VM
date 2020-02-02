module ProgramStructure where
import Data.List  (intercalate)
import Data.Maybe (fromMaybe)

import Tokens

-- Utilities
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
             , clsFields :: !Int
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
    = SubBody { varCount      :: !Int
              , subVars       :: ![VarDec]
              , subStatements :: !Statements }
instance Show SubBody where
    show sb = "<subroutineBody nVars = \""
              ++ show (varCount sb) ++ "\">\n\
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
    = LetArr !String !String !Expression !Expression
    | LetVar !String !String !Expression
    | If     !Expression !Statements (Maybe Statements) !String
    | While  !Expression !Statements !String
    | Do     !SubCall
    | Return (Maybe Expression)
instance Show Statement where
    show (LetArr array push index value) = 
        "<letStatement>\n\
        \<keyword> let </keyword>\n\
        \<identifier push = \"" ++ push
        ++ "\"> "++ array ++ " </identifier>\n\
        \<symbol> [ </symbol>\n"
        ++ shows index "\n\
        \<symbol> ] </symbol>\n\
        \<symbol> = </symbol>\n"
        ++ shows value "\n\
        \<symbol> ; </symbol>\n\
        \</letStatement>"
    show (LetVar var pop value) = "<letStatement>\n\
                                      \<keyword> let </keyword>\n\
                                      \<identifier pop = \"" ++ pop
                                      ++ "\"> " ++ var ++ " </identifier>\n\
                                      \<symbol> = </symbol>\n"
                                      ++ shows value "\n\
                                      \<symbol> ; </symbol>\n\
                                      \</letStatement>"
    show (If cond thenDo elseDo labelID) = "<ifStatement labelID = \""
                                         ++ labelID ++ "\">\n\
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
    show (While cond loop labelID) = "<whileStatement labelID = \""
                                   ++ labelID ++ "\">\n\
                                   \<keyword> while </keyword>\n\
                                   \<symbol> ( </symbol>\n"
                                   ++ shows cond "\n\
                                   \<symbol> ) </symbol>\n\
                                   \<symbol> { </symbol>\n"
                                   ++ shows loop "\n\
                                   \<symbol> } </symbol>\n\
                                   \</whileStatement>"
    show (Do subCall) = "<doStatement>\n\
                        \<keyword> do </keyword>\n"
                        ++ shows subCall    "\n\
                        \<symbol> ; </symbol>\n\
                        \</doStatement>"
    show (Return mValue) = "<returnStatement>\n\
                           \<keyword> return </keyword>\n"
                         ++ showsMaybeExp mValue
                           "<symbol> ; </symbol>\n\
                           \</returnStatement>"
      where
        showsMaybeExp (Just e) = (++) (shows e "\n")
        showsMaybeExp Nothing  = (++) ""

data SubCall = 
    SubCall !String (Maybe String) !Bool !String !String !ExpressionList
instance Show SubCall where
    show (SubCall name mPush explicitName cls sub args) = 
        let showsName = if explicitName
            then (++) ("<identifier push = \"" ++ fromMaybe "" mPush
                       ++ "\"> " ++ name ++" </identifier>\n\
                       \<symbol> . </symbol>\n")
            else (++) ""
        in showsName
           "<identifier class = \"" ++ cls 
           ++ "\"> " ++ sub ++ " </identifier>\n\
           \<symbol> ( </symbol>\n"
           ++ shows args "\n\
           \<symbol> ) </symbol>"

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
    show (Expression term1 mFollow) = "<expression>\n"
                                    ++ shows term1 "\n"
                                    ++ showsFollow mFollow
                                      "</expression>"
      where
        showsFollow (Just (op, term2)) = (++) (shows op "\n" ++ shows term2 "\n")
        showsFollow Nothing = (++) ""

data Term 
    = Call  !SubCall
    | Arr   !String !String !Expression
    | Var   !String !String
    | Unary !Token !Term
    | Const !Token
    | Parens !Expression
instance Show Term where
    show (Call subCall) = "<term>\n"
                          ++ shows subCall "\n\
                          \</term>"
    show (Arr arr push index) = "<term>\n\
                                \<identifier push = \"" ++ push
                                ++ "\"> " ++ arr ++ " </identifier>\n\
                                \<symbol> [ </symbol>\n"
                                ++ shows index "\n\
                                \<symbol> ] </symbol>\n\
                                \</term>"
    show (Var var push) = "<term>\n\
                             \<identifier push = \"" ++ push
                             ++ "\"> " ++ var ++ " </identifier>\n\
                             \</term>"
    show (Unary op term) = "<term>\n"
                           ++ shows op "\n"
                           ++ shows term "\n\
                           \</term>"
    show (Const c) = "<term>\n"
                     ++ shows c "\n\
                     \</term>"
    show (Parens expr) = "<term>\n\
                         \<symbol> ( </symbol>\n"
                         ++ shows expr "\n\
                         \<symbol> ) </symbol>\n\
                         \</term>"