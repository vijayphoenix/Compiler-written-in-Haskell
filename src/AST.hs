module AST (
    Expr(..),
    Op(..), 
    Type(..),
    Literal(..),
    Name,
    VList,
    ArgList,
    Args,
    Declaration(..),
    BinOpCall(..),
    Func(..),
    Module(..),
    FuncCall(..)
) where 

-- | Syntax rules : 
-- [A] means optional arg. of type A .

-- All the symbol starting with lower-case letter are terminal(lexer units).
-- All the operators are left associative

-- Command = Expr ;
-- Expr : DeclarationStmt | FuncCallStmt | LiteralStmt | (Expr)

-- DeclarationStmt : ExternDecl | VarDecl

-- ExternDecl : extern Name([ArgList]) : Type 
-- VarDecl    : Type VList 

-- Type : int | string 
-- VList: Name[, VList]

-- FuncCallStmt : BinOpCall | Call 
-- BinOpCall : Expr Op Expr 
-- Call : Name ( [Args] ) 

-- Op : + | - | * | / 
-- Args : Expr[, Args]

-- LiteralStmt : StrLiteral  | IntLiteral 
-- IntLiteral  : integer
-- StrLiteral  : string

-- Name : ident
-- ArgList : Type Name[, ArgList]

-- Func : def Name([ArgList]) : Type { Command-list }
-- Command-list = Command [Command-list]

-- Command = Expr ;

data Module 
    = Command Expr 
    | Method  Func 
    deriving (Show)

data Func 
    = Func {
        fname :: Name ,
        argList:: ArgList,
        retType:: Type ,
        body   :: [Expr]
    }
    deriving (Show)

data Expr
    = DeclarationStmt   Declaration
    | FuncCallStmt      FuncCall
    | BinOpCallStmt     BinOpCall
    | LiteralStmt       Literal 
    | Var               Name
    deriving (Show)


data Op 
    = Plus
    | Minus
    | Mul 
    | Divide
    deriving (Show, Ord, Eq)

data Type 
    = IntC 
    | StringC 
    deriving (Show)

data Literal 
    = StrLiteral String
    | IntLiteral Integer
    deriving (Show)


type Name = String
type VList = [Name]
type ArgList = [(Type, Name)]
type Args = [Expr]


data Declaration
    = ExternDecl {
        efName :: Name ,
        efArgsList :: ArgList,
        retT :: Type
    }
    | VarDecl { 
        vType :: Type, 
        vName :: VList
    }
    deriving (Show)


data FuncCall
    = FuncCall {
        callee :: Name ,
        args   :: Args
    }
    deriving (Show)

data BinOpCall 
    = BinOpCall {
        op     :: Op ,
        lhs    :: Expr,
        rhs    :: Expr
    } deriving (Show)


-- | For Debugging Purposes

-- instance Show Expr where 
--     show (DeclarationStmt d) = (show d) ++ ";\n"
 -- instance Show Declaration where 
 --    show c@(ExternDecl a) = show 

-- argListPrint :: ArgList -> String 
-- argListPrint [] = ""
-- argListPrint (x:farr) = (show $ fst x) ++ " " ++ (snd x) ++ ", " ++ (argListPrint farr)


-- vListPrint :: VList -> String 
-- vListPrint [] = ""
-- vListPrint (x:fx) = x ++ ", " ++ (vListPrint fx)


-- -- instance Show 

-- -- argsPrint :: Args -> String 
-- -- argsPrint [] = ""
-- -- argsPrint (x:fx) = (exprPrint x) ++ ", " ++ (argsPrint fx)

-- argsPrint :: Args -> String
-- argsPrint = show 
-- showExpr :: Expr -> String
-- showExpr = show

-- externDeclPrint (ExternDecl f a _) = "extern " ++ f ++ " -> " ++ (argListPrint a)
-- varDeclPrint (VarDecl t l) = (show t) ++ " " ++ (vListPrint l)


-- callPrint (Call c a) = "call " ++ (show c) ++ " -> " ++ (argsPrint a)
-- binOpCallPrint (BinOpCall op l r) = (show op) ++ " -> " ++ (showExpr l) ++ " " ++ (showExpr r)

-- -- Tests
-- externDecl  = ExternDecl "sin" [(IntC, "arg1")]
-- varDecl     = VarDecl StringC ["arg1", "arg2"]
-- literal     = StrLiteral "adf"
-- literalStmt = LiteralStmt literal
-- call        = Call "func" [literalStmt]
