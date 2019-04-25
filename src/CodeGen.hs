{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CodeGen where 


import LLVM.Module
import LLVM.Context

import LLVM.AST.Global
import LLVM.AST (Named ((:=)))
import qualified LLVM.AST.Type as TypeQ
import qualified LLVM.AST.Global as GLB
import qualified LLVM.AST.Linkage as Linkage
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

import LLVM.AST (Instruction(..), FastMathFlags(..))

import qualified AST as ASTp



-- -- 1. Variable Declaration Done
-- var = GLB.globalVariableDefaults { name = "Var1", GLB.type' = int }

-- | Utility Functions for Working on Module
newtype LLVM a = LLVM (State AST.Module a)
    deriving (Functor, Applicative, Monad, MonadState AST.Module )

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM mod (LLVM m) = execState m mod

-- Defines a function here the return type is Type 
define ::  AST.Type -> String -> [(AST.Type, AST.Name)] -> [AST.BasicBlock] -> LLVM ()
define retty label argtys body = addDef $
  AST.GlobalDefinition $ functionDefaults {
    name        = AST.Name label
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = body
  }

addDef :: AST.Definition -> LLVM ()
addDef def = do 
    prev <- gets AST.moduleDefinitions
    modify $ \s -> s { AST.moduleDefinitions = def:prev }


-- | BackEnd 
--   From Parsed AST to LLVM Definitions
getDef :: ASTp.Func -> AST.Definition 
getDef (ASTp.Func fname farg fret fbody ) = AST.GlobalDefinition $ functionDefaults {
                        name = getName fname,
                        parameters = ([Parameter (getType tp) (getName nm) [] | (tp, nm) <- farg], False),
                        returnType = (getType fret)
                        -- basicBlocks = getBlk fbody
                    }

getExtern :: ASTp.Declaration -> AST.Definition
getExtern (ASTp.ExternDecl fname fargs fret) = 
    let (AST.GlobalDefinition func) = (getDef (ASTp.Func fname fargs fret []))
    in (AST.GlobalDefinition $ func {
        linkage = Linkage.External
        })


getType :: ASTp.Type -> TypeQ.Type
getType ASTp.IntC = AST.IntegerType 32

getName :: ASTp.Name -> AST.Name
getName = AST.Name

intL :: TypeQ.Type
intL = AST.IntegerType 32

getArgList :: ASTp.ArgList -> [(AST.Type, AST.Name)]
getArgList = map (\(t, n) -> (getType t, getName n))

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




data CodegenState
  = CodegenState {
    currentBlock :: AST.Name                     -- Name of the active block to append to
  , blocks       :: Map.Map AST.Name BlockState  -- Blocks for function
  , symtab       :: SymbolTable              -- Function scope symbol table
  , blockCount   :: Int                      -- Count of basic blocks
  , count        :: Word                     -- Count of unnamed instructions a.k.a like function args
  , names        :: Names                    -- Name Supply
  } deriving Show

data BlockState
  = BlockState {
    idx   :: Int                            -- Block index
  , stack :: [AST.Named AST.Instruction]            -- Stack of instructions Head --> |_| |_| |_| |_|
  , term  :: Maybe (AST.Named AST.Terminator)       -- Block terminator
  } deriving Show


newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState )

sortBlocks :: [(AST.Name, BlockState)] -> [(AST.Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)

makeBlock :: (AST.Name, BlockState) -> BasicBlock
makeBlock (l, (BlockState _ s t)) = BasicBlock l (reverse s) (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: " ++ (show l)

entryBlockName :: String
entryBlockName = "entry"

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

emptyCodegen :: CodegenState
emptyCodegen = CodegenState (AST.Name entryBlockName) Map.empty [] 1 0 Map.empty

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen


------------------------------- Block Operations ------------------------

-- Modifies the current block that is being used using the block name arg.
-- Returns the input bname after successfull updation
setBlock :: AST.Name -> Codegen AST.Name
setBlock bname = do
  modify $ \s -> s { currentBlock = bname }
  return bname



-- Alloca Returns an instruction of declaration of 
-- a var of allocatedType :: Type , numElements , alignment , metadata 
alloca :: AST.Type -> Codegen AST.Operand
alloca ty = instr $ AST.Alloca ty Nothing 0 [] 


-- | Updates the count of unnamed instructions and returns total no. of unnamed instructions
fresh :: Codegen Word
fresh = do
    i <- gets count
    modify $ \s -> s { count = 1 + i }
    return $ i + 1


instr :: AST.Instruction -> Codegen (AST.Operand)
instr ins = do
    n <- fresh  -- Updated the count of variables/ n=count
    let ref = (AST.UnName n)  -- a number for a nameless thing
    blk <- current -- got the current blockState 
    let i = stack blk  -- Got the list of statements 
    modifyBlock (blk { stack = (ref := ins) : i } ) -- (ref := ins ) is a Named Instruction with name ref and ins and instruction 
    return $ local ref -- Returning the operands



current :: Codegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x -> return x
    Nothing -> error $ "No such block: " ++ show c



-- Updating the blocks with due to addition of named instructions 
-- Simply replacing whole prev block with the new one 
modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock 
  modify $ \s -> s { blocks = Map.insert active new (blocks s) }





-- Update the symbol table with the current value and identifier
assign :: String -> AST.Operand -> Codegen ()
assign var x = do
  lcls <- gets symtab
  modify $ \s -> s { symtab = [(var, x)] ++ lcls }

getvar :: String -> Codegen AST.Operand
getvar var = do
  syms <- gets symtab
  case lookup var syms of
    Just x  -> return x
    Nothing -> error $ "Local variable not in scope: " ++ show var



-- Takes the operand and create a terminator based on that . 
-- Do simply names the instruction 
-- terminator just uplift the value further to Codegen while updating the blockState
ret :: AST.Operand -> Codegen (AST.Named AST.Terminator)
ret val = terminator $ AST.Do $ AST.Ret (Just val) []


-- Update the terminator of the current active block
terminator :: AST.Named AST.Terminator -> Codegen (AST.Named AST.Terminator)
terminator trm = do
  blk <- current
  modifyBlock (blk { term = Just trm })
  return trm


-- Takes a name and gives you an operands
local ::  AST.Name -> AST.Operand
local = AST.LocalReference TypeQ.double



-- Load And Store operations
store :: AST.Operand -> AST.Operand -> Codegen AST.Operand
store ptr val = instr $ Store False ptr val Nothing 0 []

load :: AST.Operand -> Codegen AST.Operand
load ptr = instr $ Load False ptr Nothing 0 []