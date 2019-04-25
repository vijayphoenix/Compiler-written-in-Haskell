{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CodeGen where 

import LLVM.Module
import LLVM.Context

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FloatingPointPredicate as FP

import Data.Word
import Data.String
import Data.List
import Data.Function
import Data.Int
import Control.Monad.Except
import qualified Data.Map as Map

import Control.Monad.State
import Control.Applicative

import qualified AST as ASTp

import LLVM.AST
import qualified LLVM.AST as AST
import LLVM.AST.Global
import LLVM.Context
import LLVM.Module
import qualified LLVM.AST.Type as TypeQ
import qualified LLVM.AST.Global as GLB
import qualified LLVM.AST.Linkage as Linkage


-- -- 1. Variable Declaration Done
-- var = GLB.globalVariableDefaults { name = "Var1", GLB.type' = int }

-- | Utility Functions for Working on Module
newtype LLVM a = LLVM (State AST.Module a)
    deriving (Functor, Applicative, Monad, MonadState AST.Module )

addDef :: Definition -> LLVM ()
addDef def = do 
    prev <- gets moduleDefinitions
    modify $ \s -> s { moduleDefinitions = def:prev }


-- | BackEnd 
--   From Parsed AST to LLVM Definitions
getDef :: ASTp.Func -> Definition 
getDef (ASTp.Func fname farg fret fbody ) = GlobalDefinition $ functionDefaults {
                        name = Name fname,
                        parameters = ([Parameter (getType tp) (getName nm) [] | (tp, nm) <- farg], False),
                        returnType = (getType fret)
                        -- basicBlocks = getBlk fbody
                    }

getExtern :: ASTp.Declaration -> Definition
getExtern (ASTp.ExternDecl fname fargs fret) = 
    let (GlobalDefinition func) = (getDef (ASTp.Func fname fargs fret []))
    in (GlobalDefinition $ func {
        linkage = Linkage.External
        })


getType :: ASTp.Type -> TypeQ.Type
getType ASTp.IntC = IntegerType 32

getName :: ASTp.Name -> AST.Name
getName = AST.Name

-- getBlk :: [ASTp.Expr] -> [GLB.BasicBlock]
-- getBlk

type Names = Map.Map String Int


-- Takes a name and check if present in names and then checks if the name already there or not.
-- returns updated map and prev. name if any otherwise the name itself. 
-- Can be used to ask if this name is avaible and if so then use it otherwise it returns you 
-- an updated name that can be used.
unikName :: String -> Names -> (String, Names)
unikName name mapping = case Map.lookup name mapping of 
        Nothing -> (name, Map.insert name 1 mapping)
        Just idx -> (name ++ (show idx) , Map.insert name (idx+1) mapping)

type SymbolTable = [(String, AST.Operand)]




data ModuleState 
    = ModuleState {
    crntBlk  :: AST.Name,
    funcs    :: Map.Map AST.Name FuncState,
    funcCount:: Int,
    symTab   :: SymbolTable,
    count    :: Word,
    names    :: Names
} deriving (Show)

data FuncState 
    = FuncState {
    idx   :: Int ,
    insts :: [Named Instruction],
    term  :: Maybe (Named Terminator)
} deriving (Show)


newtype CodeGen a = CodeGen { runCodeGen :: State ModuleState a }
    deriving (Functor, Applicative, Monad, MonadState ModuleState)


sortBlocks :: [(Name, FuncState)] -> [(Name, FuncState)]
sortBlocks = sortBy (compare `on` (idx . snd))

createBlocks :: ModuleState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (funcs m)

makeBlock :: (Name, FuncState) -> BasicBlock
makeBlock (l, (FuncState _ s t)) = BasicBlock l (reverse s) (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: " ++ (show l)

entryBlockName :: String
entryBlockName = "entry"

emptyBlock :: Int -> FuncState
emptyBlock i = FuncState i [] Nothing

emptyCodegen :: ModuleState
emptyCodegen = ModuleState (Name entryBlockName) Map.empty 1 [] 0 Map.empty

execCodegen :: CodeGen a -> ModuleState
execCodegen m = execState (runCodeGen m) emptyCodegen


-- int :: Type
-- int = IntegerType 32

-- defAdd :: Definition
-- defAdd = GlobalDefinition functionDefaults
--   { name = Name "add"
--   , parameters =
--       ( [ Parameter int (Name "a") []
--         , Parameter int (Name "b") [] ]
--       , False )
--   , returnType = int
--   , basicBlocks = [body]
--   }
--   where
--     body = BasicBlock
--         (Name "entry")
--         [ Name "a" :=
--             Add False  -- no signed wrap
--                 False  -- no unsigned wrap
--                 (LocalReference int (Name "a"))
--                 (LocalReference int (Name "b"))
--                 []]
--         (Do $ Ret (Just (LocalReference int (Name "a"))) [])

-- -- literalStmtCodegen :: AST.Module -> AST.Module
-- -- literalStmtCodegen = 
-- -- declInt :: AST.Module -> AST.Module
-- -- declInt obj = 

-- -- mainDef :: Definition
-- -- mainDef = 

-- module_ :: AST.Module
-- module_ = defaultModule
--   { moduleName = "basic"
--   , moduleDefinitions = [defAdd]
--   }



-- toLLVM :: AST.Module -> IO AST.Module
-- toLLVM mod = withContext $ \ctx -> do
--   liftError $ withModuleFromAST ctx mod ultimate
--   return mod
--   where 
--     ultimate = \m -> do 
--       llstr <- moduleLLVMAssembly m 
--       putStrLn llstr
--       return m 
--   -- BS.putStrLn llvm

-- liftError :: ExceptT String IO a -> IO a
-- liftError = runExceptT >=> either fail return

-- exec :: IO AST.Module
-- exec = toLLVM module_




























-- import Data.ByteString.Char8 as BS

-- module_ :: AST.Module
-- module_ = AST.defaultModule {
--     AST.moduleName = "Main-Module",
--     AST.moduleDefinitions = []
--     }

-- toFound = withModuleFromAST
-- runner :: AST.Module -> IO () 
-- runner mod = withContext $ \ctx -> do
--     llvm <- withModuleFromAST ctx mod moduleLLVMAssembly
--     BS.putStrLn llvm

-- toLLVM :: AST.Module -> IO ()
-- toLLVM mod = withContext $ \ctx -> do
--   llvm <- withModuleFromAST  ctx mod moduleLLVMAssembly
--   BS.putStrLn llvm

-- toLLVM :: AST.Module -> IO AST.Module
-- toLLVM mod = withContext $ \ctx -> do
--   liftError $ withModuleFromAST ctx mod ultimate
--   return mod
--   where 
--     ultimate = \m -> do 
--       llstr <- moduleLLVMAssembly m 
--       BS.putStrLn llstr
--       return m 
--   -- BS.putStrLn llvm

-- liftError :: ExceptT String IO a -> IO a
-- liftError = runExceptT >=> either fail return