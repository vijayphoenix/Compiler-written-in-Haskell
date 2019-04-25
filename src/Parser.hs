module Parser where 

import Lexer 
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec
import qualified Text.Parsec.Token as Tok
import qualified Data.Map as Map 
import AST 


precedenceTable = Map.fromList[(Plus,200),(Minus,200),(Mul,400),(Divide,400)]

exprParser :: Parser Expr
exprParser = try binOpCallStmtParser
           <|> try factor
           <|> try declarationStmtParser

factor :: Parser Expr 
factor = try (paren exprParser)
        <|> try funcCallStmtParser
        <|> literalStmtParser

-- | Parsing type related stuffs. ----
-- NSS
typeParser :: Parser Type 
typeParser = try intTParser <|> stringTParser

-- NSS
intTParser = do 
    reserved "int"
    return IntC

-- NSS
stringTParser = do
    reserved "string"
    return StringC
-------------------------------------

opParser :: Parser Op
opParser =  try plusParser 
    <|> try minusParser 
    <|> try mulParser 
    <|> divideParser

plusParser = do
    reservedOp "+"
    return Plus

minusParser = do
    reservedOp "-"
    return Minus

mulParser = do
    reservedOp "*"
    return Mul

divideParser = do
    reservedOp "/"
    return Divide

-- | Variable Names and identifier -----
-- NSS
nameParser :: Parser Name 
nameParser = ident
----------------------------------------

-- | VList: Name[, VList] -----------------
-- NSS
vListParser :: Parser VList 
vListParser = nameParser `sepBy1` (spaces >> (char ',') >> spaces)
-------------------------------------------

-- ArgList : Type Name[, ArgList] ----------
-- NSS
argListParser :: Parser ArgList
argListParser = unit `sepBy1` (spaces >> (char ',') >> spaces)
    where unit = do
                tp    <- typeParser
                name  <- nameParser
                return (tp, name)
---------------------------------------------

-- | Args : Expr[, Args]
-- NSS
argsParser :: Parser Args 
argsParser = exprParser `sepBy1` (spaces >> (char ',') >> spaces)
-----------------------------------------------

-- delim :: Parser ()
-- delim p = (spaces >> (char p) >> spaces)



---------------------------------------------------
-- | LiteralStmt : StrLiteral  | IntLiteral 
-- NSS
literalStmtParser :: Parser Expr 
literalStmtParser = do
    res <- literalParser
    return $ LiteralStmt res
-- NSS
literalParser :: Parser Literal 
literalParser = strLiteralP <|> intLiteralP
-- NSS
strLiteralP :: Parser Literal 
strLiteralP = do 
    res <- stringLiteral
    return (StrLiteral res)
-- NSS
intLiteralP :: Parser Literal 
intLiteralP = do
    res <- integer
    return (IntLiteral res)
-----------------------------------------------------


-- | Function Call Statement
funcCallStmtParser :: Parser Expr 
funcCallStmtParser = do 
    res <- funcCallParser 
    return $ FuncCallStmt res

binOpCallStmtParser :: Parser Expr 
binOpCallStmtParser = do
    res <- binOpCallParser
    return $ BinOpCallStmt res


funcCallParser :: Parser FuncCall
funcCallParser = do 
    callee <- nameParser
    (spaces >> (char '(') >> spaces)
    args <- argsParser
    (spaces >> (char ')') >> spaces)
    return $ FuncCall callee args

-- binOpCallParser :: Parser BinOpCall
-- binOpCallParser = do 
--     lhs <- factor
--     spaces
--     op <- opParser
--     spaces
--     rhs <- exprParser
--     return $ BinOpCall op lhs rhs

-- binOpCallParser :: Parser BinOpCall
-- binOpCallParser = do 
--     temp <- factor
--     spaces
--     op <- opParser
--     spaces
--     let tokPrec = (Map.lookup op precedenceTable)
--     lhs <- rest (Just 0) tokPrec temp op
--     rhs <- exprParser
--     return $ BinOpCall op lhs rhs

-- rest:: (Maybe Int) -> (Maybe Int) -> Expr -> Op-> Parser Expr
-- rest expPrec tokPrec lhs op = (do
--             if comp tokPrec expPrec 
--                 then return lhs
--                 else do
--                     rhs <- factor
--                     spaces
--                     nextOp <- opParser
--                     spaces
--                     let nextPrec = (Map.lookup nextOp precedenceTable)
--                     if comp tokPrec nextPrec 
--                         then do
--                                 hi <- rest (Just 0) nextPrec rhs nextOp
--                                 let out = BinOpCallStmt (BinOpCall nextOp rhs hi)
--                                 return $BinOpCallStmt (BinOpCall op lhs out)
--                         else do
--                             hii <- return $ BinOpCallStmt (BinOpCall op lhs rhs)
--                             next <- rest (inc tokPrec) nextPrec hii nextOp
--                             return $BinOpCallStmt (BinOpCall nextOp hii next)
--             )


binOpCallParser :: Parser BinOpCall
binOpCallParser = do 
    temp <- factor
    spaces
    lhs <- rest (Just 0) temp
    res <- try (optionalParse lhs) <|> (singleParse lhs)
    return res


zero :: Expr
zero = (LiteralStmt (IntLiteral 0))

optionalParse :: Expr -> Parser BinOpCall
optionalParse lhs = do
    op <- opParser 
    rhs <- exprParser
    return $ BinOpCall op lhs rhs

singleParse :: Expr -> Parser BinOpCall
singleParse lhs = return $ BinOpCall Plus lhs zero

-- rest:: (Maybe Int) -> Expr -> Parser Expr
-- rest expPrec lhs= try (do
--         lop <- (lookAhead opParser)
--         let tokPrec = getTokPrec lop
--         if comp tokPrec expPrec 
--             then return lhs
--             else do
--                 op <- opParser
--                 spaces
--                 temp <- factor
--                 spaces
--                 try(do
--                     nop <-(lookAhead opParser)
--                     let nextPrec = getTokPrec nop
--                     if comp tokPrec nextPrec 
--                         then do
--                                 rhs <- rest (inc tokPrec) temp
--                                 return $ BinOpCallStmt (BinOpCall op lhs rhs)
--                         else return $ BinOpCallStmt (BinOpCall op lhs temp)
--                     )<|>return $ BinOpCallStmt (BinOpCall op lhs temp)
--                 ) <|> return lhs

getTokPrec op = (Map.lookup op precedenceTable)


rest:: (Maybe Int) -> Expr -> Parser Expr
rest expPrec lhs= try (do
        lop <- (lookAhead opParser)
        let tokPrec = getTokPrec lop
        if comp tokPrec expPrec 
            then return lhs
            else do
                op <- opParser
                spaces
                temp <- factor
                spaces
                res <- try (firstP tokPrec temp lhs op )
                         <|> (return $ BinOpCallStmt (BinOpCall op lhs temp))
                return res
            ) <|> return lhs

-- firstP :: (Maybe a) -> 
firstP tokPrec temp lhs op = do
        nop <-(lookAhead opParser)
        let nextPrec = getTokPrec nop
        if comp tokPrec nextPrec 
            then do
                rhs <- rest (inc tokPrec) temp
                return $ BinOpCallStmt (BinOpCall op lhs rhs)
            else return $ BinOpCallStmt (BinOpCall op lhs temp)


inc :: (Num a, Ord a)=> (Maybe a) -> (Maybe a)
inc (Just x) = Just (x+1)

comp :: Ord a => (Maybe a) -> (Maybe a) -> Bool
comp (Just x) (Just y) = (x < y)

-------------------------------------------------------
-- | Declaration Stuff

declarationStmtParser :: Parser Expr
declarationStmtParser = do 
    res <- declarationParser 
    return $ DeclarationStmt res


declarationParser :: Parser Declaration 
declarationParser = externDeclParser <|> varDeclParser

externDeclParser :: Parser Declaration 
externDeclParser = do 
    reserved "extern"
    spaces
    fname <- nameParser
    (spaces >> (char '(') >> spaces)
    argList <- argListParser
    (spaces >> (char ')') >> spaces >> (char ':') >> spaces)
    retT <- typeParser
    return $ ExternDecl fname argList retT 


varDeclParser :: Parser Declaration
varDeclParser = do
    t <- typeParser
    spaces
    names <- vListParser
    return $ VarDecl t names

---------------------------------------------------------

mainTest = do
    str <- getLine
    if str == "quit"
    then 
        return ()
    else do
        print (parse exprParser "sdf" str)
        mainTest



-- --------------Done
-- Type
-- Literal 
-- Name 
-- VList 
-- ArgList
-- Args
-- LiteralStmt