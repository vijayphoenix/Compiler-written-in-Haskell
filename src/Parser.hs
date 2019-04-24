module Parser where 

import Lexer 
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec
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
        -- <|> funcCallParser
        -- <|> literalParser 
        -- <|> parens exprParser

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
argListParer :: Parser ArgList
argListParer = unit `sepBy1` (spaces >> (char ',') >> spaces)
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

-- funcCallParser :: Parser 

mainTest = do
    str <- getLine
    if str == "quit"
    then 
        return ()
    else do
        print (parse argListParer "sdf" str)
        mainTest



-- --------------Done
-- Type
-- Literal 
-- Name 
-- VList 
-- ArgList
-- Args
-- LiteralStmt