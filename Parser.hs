module Parser
(parse) where
import qualified Data.Map.Strict as Map

import Tokens hiding (keywords, symbols)
import ProgramStructure

-- Export
parse :: Tokens -> Class
parse = parseClass

-- Utilities
data VarProps = VarProps { varMap  :: !String
                         , varType :: !Token }
type SymbolTable = Map.Map String VarProps

getVarNames :: [Token] -> [String]
getVarNames = helper []
  where
    helper :: [String] -> [Token] -> [String]
    helper vars (ID name:SYM ',':xs) = helper (name : vars) xs
    helper vars (ID name:SYM ';':_)  = name : vars
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
    helper [] (Map.fromList [("field", 0), ("static", 0)]) Map.empty
  where
    subKinds = ["constructor", "method", "function"]
    helper :: [ClassVarDec] -> (Map.Map String Int) -> SymbolTable -> [Token]
        -> ([ClassVarDec], SymbolTable, [Token])
    helper rvars counts symbols xs@(KW kind:typing:xs')
        | kind `elem` subKinds = (reverse rvars, symbols, xs)
        | otherwise = 
            let names = getVarNames xs'
                count = counts Map.! kind
                symbols' = updateSymbols kind typing names count symbols
                counts'  = Map.insert kind (count + length names) counts
                (consumed, ys) = splitVarTokens names xs
            in helper (ClassVarDec consumed : rvars) counts' symbols' ys
    helper rvars _ symbols [] = (reverse rvars, symbols, [])
    helper _ _ _ _ = error "Error: Class variable declaration"

parseSubs :: SymbolTable -> [Token] -> [SubDec]
parseSubs = helper []
  where
    helper :: [SubDec] -> SymbolTable -> [Token] -> [SubDec]
    helper rsubs _       (SYM '}':[]) = reverse rsubs
    helper rsubs symbols xs =
        let (sub, ys) = parseSub symbols xs
        in helper (sub : rsubs) symbols ys

parseSub :: SymbolTable -> [Token] -> (SubDec, [Token])
parseSub symbols (KW kind:typing:ID name:SYM '(':xs) =
    let (ID cls)   = varType $ symbols Map.! "class-name"
        (params, symbols', ys) = parseSubParams symbols xs
        symbols''  = Map.insert "sub-name" (VarProps "" $ ID name) symbols'
        (body, zs) = parseSubBody symbols'' ys
    in (SubDec kind typing cls name params body, zs)
parseSub _ _ = error "Error: Subroutine declaration"

parseSubParams :: SymbolTable -> [Token] -> ([Token], SymbolTable, [Token])
parseSubParams = helper [] 0
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
    let (vars, symbols',     ys) = parseSubVars symbols xs
        (statements, SYM '}':zs) = parseStatements symbols' ys
    in (SubBody vars statements, zs)
parseSubBody _ _ = error "Error: Subroutine body definition"

parseSubVars :: SymbolTable -> [Token] -> ([VarDec], SymbolTable, [Token])
parseSubVars = helper [] 0
  where
    statementKinds = ["let", "if", "while", "do", "return"]
    helper :: [VarDec] -> Int -> SymbolTable -> [Token] 
        -> ([VarDec], SymbolTable, [Token])
    helper rvars count symbols xs@(KW kind:typing:xs')
        | kind `elem` statementKinds = (reverse rvars, symbols, xs)
        | kind == "var" =
            let names = getVarNames xs'
                symbols' = updateSymbols "var" typing names count symbols
                (consumed, ys) = splitVarTokens names xs
            in helper (VarDec consumed : rvars) (count+1) symbols' ys
    helper rvars _ symbols [] = (reverse rvars, symbols, [])
    helper _ _ _ _ = error "Error: Subroutine variables declaration"

parseStatements :: SymbolTable -> [Token] -> (Statements, [Token])
parseStatements = helper [] 0
  where
    helper :: [Statement] -> Int -> SymbolTable -> [Token]
        -> (Statements, [Token])
    helper rStms _     _       xs@(SYM '}':_) = (Statements $ reverse rStms, xs)
    helper rStms count symbols xs =
        let (ID sub) = varType $ symbols Map.! "sub-name"
            label = sub ++ show count
            (newStm, ys) = case xs of
                (KW "let":_)    -> parseLet symbols xs
                (KW "if":_)     -> parseIf label symbols xs
                (KW "while":_)  -> parseWhile label symbols xs
                (KW "do":_)     -> parseDo symbols xs
                (KW "return":_) -> parseReturn symbols xs
                _               -> error "Error: Statements declaration"
        in helper (newStm : rStms) (count+1) symbols ys

parseLet :: SymbolTable -> [Token] -> (Statement, [Token])
parseLet symbols (KW "let":ID arr:SYM '[':xs) = 
    let mapping = varMap $ symbols Map.! arr
        (index, SYM ']':SYM '=':ys) = parseExpression symbols xs
        (value, SYM ';':zs) = parseExpression symbols ys
    in (LetArr arr mapping index value, zs)
parseLet symbols (KW "let":ID var:SYM '=':xs) = 
    let mapping = varMap $ symbols Map.! var
        (value, SYM ';':ys) = parseExpression symbols xs
    in (LetVar var mapping value, ys)
parseLet _ _ = error "Error: Let statement declaration"

parseIf :: String -> SymbolTable -> [Token] -> (Statement, [Token])
parseIf label symbols (KW "if":SYM '(':as) =
    let (cond, SYM ')':SYM '{':bs) = parseExpression symbols as
        (thenStmts,    SYM '}':cs) = parseStatements symbols bs
        (mElseStmts, ds) = case cs of
            KW "else":SYM '{':es -> 
                let (stms, SYM '}':fs) = parseStatements symbols es
                in (Just stms, fs)
            _ -> (Nothing, cs)
    in (If cond thenStmts mElseStmts label, ds)
parseIf _ _ _ = error "Error: If statement declaration"

parseWhile :: String -> SymbolTable -> [Token] -> (Statement, [Token])
parseWhile label symbols (KW "while":SYM '(':xs) =
    let (cond, SYM ')':SYM '{':ys) = parseExpression symbols xs
        (stms,         SYM '}':zs) = parseStatements symbols ys
    in (While cond stms label, zs)
parseWhile _ _ _ = error "Error: While statement declaration"

parseDo :: SymbolTable -> [Token] -> (Statement, [Token])
parseDo symbols xs =
    let (ID cl) = varType $ symbols Map.! "class-name"
        (doSubCall, us) = case xs of
            (KW "do":ID cls:SYM '.':ID sub:SYM '(':ys) -> (Do cls True sub, ys)
            (KW "do":ID sub:SYM '(':ys)                -> (Do cl False sub, ys)
            _ -> error "Error: Do statement declaration"
        (args, SYM ')':SYM ';':zs) = parseExpressionList symbols us
    in (doSubCall args, zs)

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
        (mFollow, zs)
            | op `elem` ops = let (term, cs) = parseTerm symbols ys'
                              in (Just (SYM op, term), cs)           
            | otherwise     = (Nothing, ys)
    in (Expression root mFollow, zs)

parseTerm :: SymbolTable -> [Token] -> (Term, [Token])
parseTerm symbols (ID cls:SYM '.':ID sub:SYM '(':xs) =
    let (args, SYM ')':ys) = parseExpressionList symbols xs
    in (Call cls sub args, ys)
parseTerm symbols (ID arr:SYM '[':xs) =
    let mapping = varMap $ symbols Map.! arr
        (index, SYM ']':ys) = parseExpression symbols xs
    in (Arr arr mapping index, ys)
parseTerm symbols (ID var:xs) =
    let mapping = varMap $ symbols Map.! var
    in (Var var mapping, xs)
parseTerm symbols (SYM '(':xs) =
    let (expr, SYM ')':ys) = parseExpression symbols xs
    in (Parens expr, ys)
parseTerm symbols (SYM op:xs)
    | op `elem` unaryOps = let (term, ys) = parseTerm symbols xs
                           in (Unary (SYM op) term, ys)
    | otherwise = error "Error: Invalid unary operator"
parseTerm _ (constant:xs) = (Const constant, xs)
parseTerm _ _ = error "Error: Term declaration"