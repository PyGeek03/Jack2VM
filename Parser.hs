module Parser
(parse) where
import qualified Data.Map.Strict as Map
import Data.Char (isUpper)

import Tokens
import ProgramStructure

-- Export
parse :: Tokens -> Class
parse = parseClass

-- Utilities
data VarProps = VarProps { varMap   :: !String
                         , varToken :: !Token }
type SymbolTable = Map.Map String VarProps

getVarNames :: [Token] -> [String]
getVarNames = helper []
  where
    helper :: [String] -> [Token] -> [String]
    helper rvars (ID name:SYM ',':xs) = helper (name : rvars) xs
    helper rvars (ID name:SYM ';':_)  = reverse $ name : rvars
    helper _ _ = error "Error: Variable name declaration"

splitVarTokens :: [String] -> [Token] -> ([Token], [Token])
splitVarTokens vars =
    let varTokensLength = 2 + 2 * length vars
    in splitAt varTokensLength

updateSymbols :: String -> Token -> [String] -> Int -> SymbolTable
    -> SymbolTable
updateSymbols kind typing names startingIndex oldTable = 
    let segment = case kind of
            "field"  -> "this "
            "var"    -> "local "
            "static" -> "static "
            _ -> error "Error: Variable kind declaration"
        zipper = zipWith $ \x y -> (x, VarProps (segment ++ show y) typing)
        list = zipper names [startingIndex..]
        newTable = Map.fromList list
    in Map.union newTable oldTable

-- Main functionalities
parseClass :: Tokens -> Class
parseClass (Tokens (KW "class":ID name:SYM '{':xs)) =
    let (vars, symbols, ys) = parseClassVars xs
        symbols' = Map.insert "class-name" (VarProps "" $ ID name) symbols
        subs = parseSubs symbols' ys
    in Class name vars subs
parseClass _ = error "Error: Class declaration"

parseClassVars :: [Token] -> ([ClassVarDec], SymbolTable, [Token])
parseClassVars = 
    helper [] (Map.fromList [("field", VarProps "" $ IC 0), ("static", VarProps "" $ IC 0)])
  where
    subKinds = ["constructor", "method", "function"]
    helper :: [ClassVarDec] -> SymbolTable -> [Token]
        -> ([ClassVarDec], SymbolTable, [Token])
    helper rvars symbols xs@(KW kind:typing:xs')
        | kind `elem` subKinds = (reverse rvars, symbols, xs)
        | otherwise =
            let names = getVarNames xs'
                (IC count) = varToken $ symbols Map.! kind
                symbols'  = updateSymbols kind typing names count symbols
                newCount  = count + length names
                symbols'' = Map.insert kind (VarProps "" $ IC newCount) symbols'
                (consumed, ys) = splitVarTokens names xs
            in helper (ClassVarDec consumed : rvars) symbols'' ys
    helper rvars symbols [] = (reverse rvars, symbols, [])
    helper _ _ _ = error "Error: Class variable declaration"

parseSubs :: SymbolTable -> [Token] -> [SubDec]
parseSubs = helper []
  where
    helper :: [SubDec] -> SymbolTable -> [Token] -> [SubDec]
    helper rsubs _       (SYM '}':_) = reverse rsubs
    helper rsubs symbols xs =
        let (sub, ys) = parseSub symbols xs
        in helper (sub : rsubs) symbols ys

parseSub :: SymbolTable -> [Token] -> (SubDec, [Token])
parseSub symbols (KW kind:typing:ID name:SYM '(':xs) =
    let (ID cls)   = varToken $ symbols Map.! "class-name"
        (IC fieldCount) = varToken $ symbols Map.! "field"
        (params, symbols', ys) = case kind of
            "method" -> parseSubParams 1 symbols xs
            _        -> parseSubParams 0 symbols xs
        symbols''  = Map.insert "sub-name" (VarProps "" $ ID name) symbols'
        (body, zs) = parseSubBody symbols'' ys
    in (SubDec kind typing cls fieldCount name params body, zs)
parseSub _ _ = error "Error: Subroutine declaration"

parseSubParams :: Int -> SymbolTable -> [Token] -> ([Token], SymbolTable, [Token])
parseSubParams = helper []
  where
    helper :: [Token] -> Int -> SymbolTable -> [Token]
        -> ([Token], SymbolTable, [Token])
    helper rparams count symbols xs = case xs of
        SYM ',':ys -> helper (SYM ',' : rparams) count symbols ys
        SYM ')':ys -> (reverse rparams, symbols, ys)
        typing:ID name:ys -> 
            let rparams' = (ID name) : typing : rparams
                mapping  = "argument " ++ show count
                symbols' = Map.insert name (VarProps mapping typing) symbols
            in helper rparams' (count+1) symbols' ys
        _ -> error "Error: Subroutine parameters declaration"

parseSubBody :: SymbolTable -> [Token] -> (SubBody, [Token])
parseSubBody symbols (SYM '{':xs) =
    let (count, vars, symbols',     ys) = parseSubVars symbols xs
        (statements, SYM '}':zs) = parseStatements symbols' ys
    in (SubBody count vars statements, zs)
parseSubBody _ _ = error "Error: Subroutine body definition"

parseSubVars :: SymbolTable -> [Token] -> (Int, [VarDec], SymbolTable, [Token])
parseSubVars = helper [] 0
  where
    statementKinds = ["let", "if", "while", "do", "return"]
    helper :: [VarDec] -> Int -> SymbolTable -> [Token] 
        -> (Int, [VarDec], SymbolTable, [Token])
    helper rvars count symbols xs@(KW kind:typing:xs')
        | kind `elem` statementKinds = (count, reverse rvars, symbols, xs)
        | kind == "var" =
            let names = getVarNames xs'
                symbols' = updateSymbols "var" typing names count symbols
                count' = count + length names
                (consumed, ys) = splitVarTokens names xs
            in helper (VarDec consumed : rvars) count' symbols' ys
    helper rvars count symbols [] = (count, reverse rvars, symbols, [])
    helper _ _ _ _ = error "Error: Subroutine variables declaration"

parseStatements :: SymbolTable -> [Token] -> (Statements, [Token])
parseStatements = helper [] 0
  where
    helper :: [Statement] -> Int -> SymbolTable -> [Token]
        -> (Statements, [Token])
    helper rStms _ _ xs@(SYM '}':_) = (Statements $ reverse rStms, xs)
    helper rStms count symbols xs =
        let (ID sub) = varToken $ symbols Map.! "sub-name"
            labelID = sub ++ "." ++ show count
            symbols' = Map.insert "sub-name" (VarProps "" $ ID labelID) symbols
            (newStm, ys) = case xs of
                (KW "let":_)    -> parseLet symbols' xs
                (KW "if":_)     -> parseIf labelID symbols' xs
                (KW "while":_)  -> parseWhile labelID symbols' xs
                (KW "do":_)     -> parseDo symbols' xs
                (KW "return":_) -> parseReturn symbols' xs
                _               -> error "Error: Statements declaration"
        in helper (newStm : rStms) (count+1) symbols ys

parseLet :: SymbolTable -> [Token] -> (Statement, [Token])
parseLet symbols (KW "let":ID arr:SYM '[':xs) = 
    let push = "push " ++ varMap (symbols Map.! arr)
        (index, SYM ']':SYM '=':ys) = parseExpression symbols xs
        (value, SYM ';':zs) = parseExpression symbols ys
    in (LetArr arr push index value, zs)
parseLet symbols (KW "let":ID var:SYM '=':xs) = 
    let pop = "pop " ++ varMap (symbols Map.! var)
        (value, SYM ';':ys) = parseExpression symbols xs
    in (LetVar var pop value, ys)
parseLet _ _ = error "Error: Let statement declaration"

parseIf :: String -> SymbolTable -> [Token] -> (Statement, [Token])
parseIf labelID symbols (KW "if":SYM '(':as) =
    let (cond, SYM ')':SYM '{':bs) = parseExpression symbols as
        (thenStmts,    SYM '}':cs) = parseStatements symbols bs
        (mElseStmts, ds) = case cs of
            KW "else":SYM '{':es -> 
                let (stms, SYM '}':fs) = parseStatements symbols es
                in (Just stms, fs)
            _ -> (Nothing, cs)
    in (If cond thenStmts mElseStmts labelID, ds)
parseIf _ _ _ = error "Error: If statement declaration"

parseWhile :: String -> SymbolTable -> [Token] -> (Statement, [Token])
parseWhile labelID symbols (KW "while":SYM '(':xs) =
    let (cond, SYM ')':SYM '{':ys) = parseExpression symbols xs
        (stms,         SYM '}':zs) = parseStatements symbols ys
    in (While cond stms labelID, zs)
parseWhile _ _ _ = error "Error: While statement declaration"

parseDo :: SymbolTable -> [Token] -> (Statement, [Token])
parseDo symbols (KW "do":ID name:SYM '.':ID sub:SYM '(':xs) =
    let nameIsClass = isUpper $ head name
        (ID cls, mPush)
            | nameIsClass = (ID name, Nothing)
            | otherwise   = let var = symbols Map.! name
                            in (varToken var, Just $ "push " ++ varMap var)
        (args, SYM ')':SYM ';':ys) = parseExpressionList symbols xs
        subCall = SubCall name mPush True cls sub args
    in (Do subCall, ys)
parseDo symbols (KW "do":ID sub:SYM '(':xs) =
    let (ID cls) = varToken $ symbols Map.! "class-name"
        (args, SYM ')':SYM ';':ys) = parseExpressionList symbols xs
        subCall = SubCall cls (Just "push pointer 0") False cls sub args
    in (Do subCall, ys)
parseDo _ _ = error "Error: Do statement declaration"

parseReturn :: SymbolTable -> [Token] -> (Statement, [Token])
parseReturn _       (KW "return":SYM ';':xs) = (Return Nothing, xs)
parseReturn symbols (KW "return":xs) =
    let (expr, SYM ';':ys) = parseExpression symbols xs
    in (Return $ Just expr, ys)
parseReturn _ _ = error "Error: Return statement declaration"

parseExpressionList :: SymbolTable -> [Token] -> (ExpressionList, [Token])
parseExpressionList = helper []
  where
    helper :: [Expression] -> SymbolTable -> [Token]
        -> (ExpressionList, [Token])
    helper rexpr symbols    (SYM ',':xs) = helper rexpr symbols xs
    helper rexpr _       xs@(SYM ')':_)  = 
        (ExpressionList $ reverse rexpr, xs)
    helper rexpr symbols xs =
        let (newExpr, ys) = parseExpression symbols xs
        in helper (newExpr : rexpr) symbols ys

parseExpression :: SymbolTable -> [Token] -> (Expression, [Token])
parseExpression symbols xs =
    let (root, ys@(SYM op:ys')) = parseTerm symbols xs
        ops = "+-*/&|<>="
        (mFollow, zs)
            | op `elem` ops = let (term, cs) = parseTerm symbols ys'
                              in (Just (SYM op, term), cs)           
            | otherwise     = (Nothing, ys)
    in (Expression root mFollow, zs)

parseTerm :: SymbolTable -> [Token] -> (Term, [Token])
parseTerm symbols (ID name:SYM '.':ID sub:SYM '(':xs) =
    let nameIsClass = isUpper $ head name
        (ID cls, mPush)
            | nameIsClass = (ID name, Nothing)
            | otherwise   = let var = symbols Map.! name
                            in (varToken var, Just $ "push " ++ varMap var)
        (args, SYM ')':ys) = parseExpressionList symbols xs
        subCall = SubCall name mPush True cls sub args
    in (Call subCall, ys)
parseTerm symbols (ID sub:SYM '(':xs) =
    let (ID cls) = varToken $ symbols Map.! "class-name"
        (args, SYM ')':ys) = parseExpressionList symbols xs
        subCall = SubCall cls (Just "push pointer 0") False cls sub args
    in (Call subCall, ys)
parseTerm symbols (ID arr:SYM '[':xs) =
    let push = "push " ++ varMap (symbols Map.! arr)
        (index, SYM ']':ys) = parseExpression symbols xs
    in (Arr arr push index, ys)
parseTerm symbols (ID var:xs) =
    (Var var $ "push " ++ varMap (symbols Map.! var), xs)
parseTerm symbols (SYM '(':xs) =
    let (expr, SYM ')':ys) = parseExpression symbols xs
    in (Parens expr, ys)
parseTerm symbols (SYM op:xs)
    | op `elem` unaryOps = let (term, ys) = parseTerm symbols xs
                           in (Unary (SYM op) term, ys)
    | otherwise = error "Error: Invalid unary operator"
  where unaryOps = ['-','~']
parseTerm _ (constant:xs) = (Const constant, xs)
parseTerm _ _ = error "Error: Term declaration"