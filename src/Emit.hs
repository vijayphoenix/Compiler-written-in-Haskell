{-# LANGUAGE OverloadedStrings #-}

module Emit where 

import AST as ASTp
import CodeGen as CG

import Control.Monad.Except

import qualified Data.Map as Map


import LLVM.Context
import LLVM.Module
import LLVM.AST (Instruction(..), FastMathFlags(..), Operand(..))
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Attribute as A


import Control.Monad.State
import Control.Applicative

-- | Parser AST to LLVM code

funcGen :: ASTp.Func -> LLVM ()
funcGen x = addDef $ getDef x

declarationGen :: ASTp.Declaration -> LLVM ()
declarationGen c@(ASTp.ExternDecl _ _ _) = addDef $ getExtern c

codegenTop :: ASTp.Func -> LLVM ()
codegenTop (ASTp.Func fname fargs fret fbody) = do
  define (getType fret) fname fnargs bls
  where
    fnargs = getArgList fargs
    bls = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName  -- entryBlockName is just "entry"::String / entry is just the name to refer to the block
      setBlock entry
      forM fargs (\a -> varDeclGen a)
      varGen (fbody!!0) >>= ret  -- cgen body ~ Codegen Operand for (Float 1.0)



varDeclGen :: (ASTp.Type, ASTp.Name) -> Codegen ()
varDeclGen a = do -- Var contains the (LocalReference Type Name)
    var <- alloca (getType (fst a))
    store var (local (getName (snd a)))-- Store the value in the variable 
    assign (snd a) var-- Update the symbol table


-- -- Create a default block with the name and then returns the name for future reference 
addBlock :: String -> Codegen AST.Name
addBlock bname = do  -- bname is just "entry"::String 
  bls <- gets blocks  -- bls = A map from Name to blockState 
  ix  <- gets blockCount -- ix = integer of type int
  nms <- gets names  -- All the names 

  let new = emptyBlock ix -- new = { ix, [], Nothing }
      (qname, supply) = unikName bname nms

  modify $ \s -> s { 
                     CG.blocks = Map.insert (AST.Name qname) new bls -- updated the blocks list by inserting a new block of name qname, with block new and 
                   , CG.blockCount = ix + 1
                   , CG.names = supply
                   }
  return (AST.Name qname) -- Now this name can be used to refer to the block 

binops = Map.fromList [
      (ASTp.Plus, fadd)
    , (ASTp.Minus, fsub)
    , (ASTp.Mul, fmul)
    , (ASTp.Divide, fdiv)
  ]


-- Arithmetic and Constants
fadd :: AST.Operand -> AST.Operand -> Codegen AST.Operand
fadd a b = instr $ FAdd NoFastMathFlags a b []

fsub :: AST.Operand -> AST.Operand -> Codegen AST.Operand
fsub a b = instr $ FSub NoFastMathFlags a b []

fmul :: AST.Operand -> AST.Operand -> Codegen AST.Operand
fmul a b = instr $ FMul NoFastMathFlags a b []

fdiv :: AST.Operand -> AST.Operand -> Codegen AST.Operand
fdiv a b = instr $ FDiv NoFastMathFlags a b []

externf :: AST.Name -> AST.Operand
externf = ConstantOperand . C.GlobalReference intL

-- | Actions associated with each branch of AST
-- | Used to refer to a variable
varGen :: ASTp.Expr -> Codegen AST.Operand
varGen (Var x) = getvar x >>= load

literalGen :: ASTp.Literal -> Codegen AST.Operand
literalGen (ASTp.IntLiteral n) = return $ ConstantOperand $ (C.Int 32 n)

-- Effects
call :: AST.Operand -> [AST.Operand] -> Codegen AST.Operand
call fn args = instr $ Call Nothing CC.C [] (Right fn) (toArgs args) [] []


toArgs :: [AST.Operand] -> [(AST.Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))


funcCallGen :: ASTp.FuncCall -> Codegen AST.Operand
funcCallGen (FuncCall callee args) = do
    largs <- mapM exprGen args
    call (externf (AST.Name callee)) largs

exprGen :: ASTp.Expr -> Codegen AST.Operand
exprGen (FuncCallStmt f) = funcCallGen f
-- exprGen (BinOpCallStmt s)= 
exprGen (LiteralStmt st) = literalGen st



-- withContext == Create a Context, run an action (to which it is provided), then destroy the Context.
codegen :: AST.Module -> [ASTp.Func] -> IO AST.Module
codegen mod fns = withContext $ \context -> -- fns  = [(Function "foo " ["a", "b"] (Float 1.0)) ]
  liftError $ withModuleFromAST context newast $ \m -> do
    llstr <- moduleLLVMAssembly m
    putStrLn llstr
    return newast
  where
    modn    = mapM codegenTop fns  -- list of LLVM ()
    newast  = runLLVM mod modn  


liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return