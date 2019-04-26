module Test where

import AST as ASTp
import LLVM.AST as ASTL
import CodeGen
import Emit
import Language
import Lexer
import Main
import Parser

import LLVM.Context
import LLVM.Module

p :: LLVM [()]
p = y' z'

p' :: ASTL.Module
p' = x' firstModule p

-- check :: ASTL.Module ->  IO ASTL.Module
check x = withContext $ \context ->
    liftError $ withModuleFromAST context x $ \m -> do
    llstr <- moduleLLVMAssembly m
    putStrLn llstr

-- varDeclGen :: (ASTp.Type, ASTp.Name) -> Codegen ()
-- varDeclGen a = do -- Var contains the (LocalReference Type Name)
--     var <- alloca (getType (fst a))
--     store var (local (getName (snd a)))-- Store the value in the variable 
--     assign (snd a) var                  -- Update the symbol table