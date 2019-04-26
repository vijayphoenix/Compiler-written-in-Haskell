{-# LANGUAGE OverloadedStrings #-}

module Emit (
    addBlock,
    
    fadd,
    fsub,
    fmul,
    fdiv,

    exprGen,
    binOpCallGen,
    declarationGen,
    codegenTop,
    codegen,
    varDeclGen,
    literalGen,
    moduleGen,
    call,
    toArgs,

    x',
    y',
    funcCallGen,

)where 

import AST as ASTp
import CodeGen as CG

import Control.Monad.Except

import qualified Data.Map as Map


import LLVM.Context
import LLVM.Module
import LLVM.AST (Instruction(..), FastMathFlags(..), Operand(..))
import qualified LLVM.AST as ASTL
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Attribute as A


import Control.Monad.State
import Control.Applicative

-- | Parser ASTL to LLVM code


-- -- Create a default block with the name and then returns the name for future reference 
addBlock :: String -> Codegen ASTL.Name
addBlock bname = do  -- bname is just "entry"::String 
  bls <- gets blocks  -- bls = A map from Name to blockState 
  ix  <- gets blockCount -- ix = integer of type int
  nms <- gets names  -- All the names 

  let new = emptyBlock ix -- new = { ix, [], Nothing }
      (qname, supply) = unikName bname nms

  modify $ \s -> s { 
                     CG.blocks = Map.insert (ASTL.Name qname) new bls -- updated the blocks list by inserting a new block of name qname, with block new and 
                   , CG.blockCount = ix + 1
                   , CG.names = supply
                   }
  return (ASTL.Name qname) -- Now this name can be used to refer to the block 

binops = Map.fromList [
      (ASTp.Plus, fadd)
    , (ASTp.Minus, fsub)
    , (ASTp.Mul, fmul)
    , (ASTp.Divide, fdiv)
  ]


-- Arithmetic and Constants
fadd :: ASTL.Operand -> ASTL.Operand -> Codegen ASTL.Operand
fadd a b = instr $ FAdd NoFastMathFlags a b []

fsub :: ASTL.Operand -> ASTL.Operand -> Codegen ASTL.Operand
fsub a b = instr $ FSub NoFastMathFlags a b []

fmul :: ASTL.Operand -> ASTL.Operand -> Codegen ASTL.Operand
fmul a b = instr $ FMul NoFastMathFlags a b []

fdiv :: ASTL.Operand -> ASTL.Operand -> Codegen ASTL.Operand
fdiv a b = instr $ FDiv NoFastMathFlags a b []

externf :: ASTL.Name -> ASTL.Operand
externf = ConstantOperand . C.GlobalReference intL

-- | Actions associated with each branch of ASTL
-- | Used to refer to a variable


exprGen :: ASTp.Expr -> Codegen ASTL.Operand
exprGen (Var x) = getvar x >>= load
exprGen (FuncCallStmt f) = funcCallGen f
exprGen (LiteralStmt st) = literalGen st
exprGen (BinOpCallStmt st) = binOpCallGen st
exprGen (DeclarationStmt st) = declarationGen st

binOpCallGen :: ASTp.BinOpCall -> Codegen ASTL.Operand
binOpCallGen (BinOpCall op lhs rhs) = do 
    case Map.lookup op binops of 
        Just func -> do 
            ca <- exprGen lhs
            cb <- exprGen rhs
            func ca cb
        Nothing -> error "No Such Operation"

declarationGen :: ASTp.Declaration -> Codegen ASTL.Operand
declarationGen (VarDecl t l) = do 
    varDeclGen (t, l!!0)
    -- forM l (\x -> varDeclGen (t, x))
    count <- literalGen (ASTp.IntLiteral (toInteger (length l)))
    return count

varDeclGen :: (ASTp.Type, ASTp.Name) -> Codegen ()
varDeclGen a = do -- Var contains the (LocalReference Type Name)
    var <- alloca (getType (fst a))
    store var (local (getName (snd a)))-- Store the value in the variable 
    assign (snd a) var                  -- Update the symbol table



literalGen :: ASTp.Literal -> Codegen ASTL.Operand
literalGen (ASTp.IntLiteral n) = return $ ConstantOperand $ (C.Int 32 n)

-- Effects
call :: ASTL.Operand -> [ASTL.Operand] -> Codegen ASTL.Operand
call fn args = instr $ Call Nothing CC.C [] (Right fn) (toArgs args) [] []


toArgs :: [ASTL.Operand] -> [(ASTL.Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))


funcCallGen :: ASTp.FuncCall -> Codegen ASTL.Operand
funcCallGen (FuncCall callee args) = do
    largs <- mapM exprGen args
    call (externf (ASTL.Name callee)) largs



y' :: Traversable t => t Func -> LLVM (t ())
y' fns = mapM codegenTop fns

x' :: ASTL.Module -> LLVM a -> ASTL.Module
x' mod modn = runLLVM mod modn



-- withContext == Create a Context, run an action (to which it is provided), then destroy the Context.
codegen :: ASTL.Module -> [ASTp.Func] -> IO ASTL.Module
codegen mod fns = withContext $ \context -> -- fns  = [(Function "foo " ["a", "b"] (Float 1.0)) ]
  liftError $ withModuleFromAST context newast $ \m -> do
    llstr <- moduleLLVMAssembly m
    putStrLn llstr
    return newast
  where
    modn    = mapM codegenTop fns  -- list of LLVM ()
    newast  = runLLVM mod modn  



-- | Parser Module to LLVM Module
moduleGen :: ASTL.Module -> ASTp.Module -> IO ASTL.Module
moduleGen modL (ASTp.Method func) = codegen modL [func]
moduleGen modL (ASTp.Command expr) = codegen modL [rest] 
    where rest = ASTp.Func {
        ASTp.fname = "main",
        ASTp.argList = [],
        ASTp.retType = ASTp.IntC,
        ASTp.body = [expr]
    }

codegenTop :: ASTp.Func -> LLVM ()
codegenTop (ASTp.Func fname fargs fret fbody) = do
  define (getType fret) fname fnargs bls
  where
    fnargs = getArgList fargs
    bls = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName  -- entryBlockName is just "entry"::String / entry is just the name to refer to the block
      setBlock entry
      forM fargs (\a -> varDeclGen a)
      exprGen (fbody!!0) >>= ret  -- cgen body ~ Codegen Operand for (Float 1.0)


liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return



-- funcGen :: ASTp.Func -> LLVM ()
-- funcGen x = addDef $ getDef x

-- declarationGen :: ASTp.Declaration -> LLVM ()
-- declarationGen c@(ASTp.ExternDecl _ _ _) = addDef $ getExtern c