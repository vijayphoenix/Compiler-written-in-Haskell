module Parser where 

import Lexer 
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec
import qualified Text.Parsec.Token as Tok
import qualified Data.Map as Map 
import AST 


precedenceTable = Map.fromList[("Plus",20),("Minus",20),("Mul",40),("Divide",40)]

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

binOpCallParser :: Parser BinOpCall
binOpCallParser = do 
    lhs <- factor
    spaces
    op <- opParser
    spaces
    rhs <- exprParser
    return $ BinOpCall op lhs rhs

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