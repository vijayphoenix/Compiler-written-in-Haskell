module Parser (
    moduleParser,
    funcParser,
    ifthenParser,
    binOpCallStmtParser,
    binOpCallParser,
    literalStmtParser,
    funcCallStmtParser
)where 

import Lexer 
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec
import qualified Text.Parsec.Token as Tok
import qualified Data.Map as Map 
import AST 

moduleParser :: Parser Module 
moduleParser = spaces >> ( try methodParser
           <|> commandParser )


methodParser :: Parser Module
methodParser = do 
    res <- funcParser 
    return $ Method res 

commandParser :: Parser Module 
commandParser = do
    res <- exprParser
    spaces
    char ';'
    return $ Command res 

funcParser :: Parser Func 
funcParser = do 
    reserved "def"
    spaces
    fname <- nameParser
    (spaces >> (char '(') >> spaces)
    argList <- argListParser
    (spaces >> (char ')') >> spaces >> (char ':') >> spaces)
    retT <- typeParser
    (spaces >> (char '{') >> spaces)
    body <- (exprParser `endBy1` (spaces >> (char ';') >> spaces))
    (spaces >> (char '}') >> spaces)
    return $ Func fname argList retT body


exprParser :: Parser Expr
exprParser = try primary
           <|> ifthenStmtParser
           <|> declarationStmtParser

variableParser :: Parser Expr
variableParser = do 
    res <- nameParser
    return $ Var res

factor :: Parser Expr 
factor = try (paren exprParser)
        <|> try funcCallStmtParser
        <|> try literalStmtParser
        <|> variableParser

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
funcCallParser = do 
    callee <- nameParser
    (spaces >> (char '(') >> spaces)
    args <- argsParser
    (spaces >> (char ')') >> spaces)
    return $ FuncCall callee args


binOpCallStmtParser :: Parser Expr 
binOpCallStmtParser = do
    res <- binOpCallParser
    return $ BinOpCallStmt res

primary :: Parser Expr 
primary = do
    res <- factor
    spaces 
    lop <- try (lookAhead opParser) <|> return Null
    if lop == Null 
        then return res
    else do 
        lhs <- try (func res) <|> (return res)
        return lhs


binOpCallParser :: Parser BinOpCall
binOpCallParser = do 
    temp <- factor
    spaces
    lhs <- try (func temp) <|> (return temp)
    a <- singleParse lhs
    return a

func :: Expr -> Parser Expr
func lhs = do
    res <- rest (Just 0) lhs
    res' <- try (withLookopParser res) <|> return res
    return res'

withLookopParser :: Expr -> Parser Expr
withLookopParser res = do
    op <- lookAhead opParser
    res' <- func res
    return res'


singleParse :: Expr -> Parser BinOpCall
singleParse (BinOpCallStmt a) = return a
singleParse lhs = return $ BinOpCall Plus lhs (LiteralStmt (IntLiteral 0))

precedenceTable = Map.fromList[(Plus,10),(Minus,10),(Mul,20),(Divide,20)]

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

ifthenStmtParser :: Parser Expr 
ifthenStmtParser = do
    res <- ifthenParser
    return $ IfthenStmt res

ifthenParser :: Parser Ifthen
ifthenParser = do
    reserved "if"
    (spaces >> (char '(') >> spaces)
    cond <- exprParser
    (spaces >> (char ')') >> spaces)
    reserved "then"
    (spaces >> (char '{') >> spaces)
    tr <- exprParser
    (spaces >> (char '}') >> spaces)
    reserved "else"
    (spaces >> (char '{') >> spaces)
    fl <- exprParser
    (spaces >> (char '}') >> spaces)
    return $ Ifthen cond tr fl