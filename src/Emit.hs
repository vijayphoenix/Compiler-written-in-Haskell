{-# LANGUAGE OverloadedStrings #-}

module Emit where 


-- | Parser AST to LLVM code

-- nameGen :: Name -> LLVM ()

-- codegenTop :: S.Expr -> LLVM ()  --Stack 1.1
-- codegenTop (S.Function name args body) = do  -- args = ["a", "b"]
--   define double name fnargs bls
--   where
--     fnargs = toSig args
--     bls = createBlocks $ execCodegen $ do
--       entry <- addBlock entryBlockName  -- entryBlockName is just "entry"::String / entry is just the name to refer to the block
--       setBlock entry
--       forM args $ \a -> do -- For each argument we are assigning a variable
--         var <- alloca double -- Var contains the (LocalReference Type Name)
--         store var (local (AST.Name a)) -- Store the value in the variable 
--         assign a var -- Update the symbol table
--       cgen body >>= ret  -- cgen body ~ Codegen Operand for (Float 1.0)

-- codegenTop (S.Extern name args) = do
--   external double name fnargs
--   where fnargs = toSig args