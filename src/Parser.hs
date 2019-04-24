module Parser where 

import Lexer 
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec
import qualified Text.Parsec.Token as Tok
import AST 

-- literalParser :: Type -> Parser Literal 
-- literalParser IntC = do 
--     val <- integer
--     return (IntLiteral val)
-- literalParser StringC = do 
--     val <- stringLiteral
--     return (StrLiteral val)

-- strP = literalParser StringC

-- topParser :: Parser Module
-- topParser = commandParser <|> funcParser

-- commandParser :: Parser Module 
-- commandParser = do 
--     spaces
--     exp <- exprParser
--     char ';'
--     spaces
--     return $ Command exp



exprParser :: Parser Expr
exprParser = literalStmtParser
           <|> declarationStmtParser
           <|> funcCallStmtParser
           <|> (Tok.parens lexer exprParser)

-- declStmtParser :: Parser Expr 
-- declStmtParser = do
--     decl <- declParser
--     return $ DeclarationStmt decl

-- declParser :: Parser Declaration
-- declParser = externDeclParser <|> varDeclParser

-- externDeclParser :: Parser Declaration
-- externDeclParser = do
--     spaces
--     string "extern"
--     name <- nameParser
--     argList <- argListParser
--     ret <- typeParser
--     spaces
--     return $ ExternDecl name argList ret 

-- varDeclParser :: Parser Declaration
-- varDeclParser = do 
--     spaces
--     type <- typeParser
--     nameList <- vListParser
--     spaces
--     return $ VarDecl type nameList


-- | Parsing type related stuffs. ----
-- NSS
typeParser :: Parser Type 
typeParser = intTParser <|> stringTParser

-- NSS
intTParser = do 
    reserved "int"
    spaces
    return IntC

-- NSS
stringTParser = do
    reserved "string"
    spaces
    return StringC
-------------------------------------

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

funcCallParser :: Parser FuncCall 
funcCallParser = callParser {-<|> binOpCall-}

callParser :: Parser FuncCall
callParser = do 
    callee <- nameParser
    (spaces >> (char '(') >> spaces)
    args <- argsParser
    (spaces >> (char ')') >> spaces)
    return $ Call callee args



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

-- funcCallParser :: Parser 


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