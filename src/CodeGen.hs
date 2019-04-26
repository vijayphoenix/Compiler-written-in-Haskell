{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CodeGen (
  LLVM(..),
  runLLVM,
  define,
  addDef,
  getDef,
  getType, 
  getName,
  intL,
  getArgList,
  Names(..),
  load, 
  store,
  local,
  terminator,
  ret,
  getvar,
  assign,
  createBlocks,
  Codegen(..),
  CodegenState(..),
  BlockState(..),
  instr,
  alloca,
  execCodegen,
  setBlock,
  emptyBlock,
  unikName,
  entryBlockName

)where 


import LLVM.Module
import LLVM.Context

import LLVM.AST.Global
import LLVM.AST (Named ((:=)))
import qualified LLVM.AST.Type as TypeQ
import qualified LLVM.AST.Global as GLB
import qualified LLVM.AST.Linkage as Linkage
import qualified LLVM.AST as ASTL
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

import qualified AST as ASTLp



-- -- 1. Variable Declaration Done
-- var = GLB.globalVariableDefaults { name = "Var1", GLB.type' = int }

-- | Utility Functions for Working on Module
newtype LLVM a = LLVM (State ASTL.Module a)
    deriving (Functor, Applicative, Monad, MonadState ASTL.Module )

runLLVM :: ASTL.Module -> LLVM a -> ASTL.Module
runLLVM mod (LLVM m) = execState m mod

-- Defines a function here the return type is Type 
define ::  ASTL.Type -> String -> [(ASTL.Type, ASTL.Name)] -> [ASTL.BasicBlock] -> LLVM ()
define retty label argtys body = addDef $
  ASTL.GlobalDefinition $ functionDefaults {
    name        = ASTL.Name label
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = body
  }

addDef :: ASTL.Definition -> LLVM ()
addDef def = do 
    prev <- gets ASTL.moduleDefinitions
    modify $ \s -> s { ASTL.moduleDefinitions = prev ++ [def] }


-- | BackEnd 
--   From Parsed ASTL to LLVM Definitions
getDef :: ASTLp.Func -> ASTL.Definition 
getDef (ASTLp.Func fname farg fret fbody ) = ASTL.GlobalDefinition $ functionDefaults {
                        name = getName fname,
                        parameters = ([Parameter (getType tp) (getName nm) [] | (tp, nm) <- farg], False),
                        returnType = (getType fret)
                        -- basicBlocks = getBlk fbody
                    }

getExtern :: ASTLp.Declaration -> ASTL.Definition
getExtern (ASTLp.ExternDecl fname fargs fret) = 
    let (ASTL.GlobalDefinition func) = (getDef (ASTLp.Func fname fargs fret []))
    in (ASTL.GlobalDefinition $ func {
        linkage = Linkage.External
        })


getType :: ASTLp.Type -> TypeQ.Type
getType ASTLp.IntC = ASTL.IntegerType 32

getName :: ASTLp.Name -> ASTL.Name
getName = ASTL.Name

intL :: TypeQ.Type
intL = ASTL.IntegerType 32

getArgList :: ASTLp.ArgList -> [(ASTL.Type, ASTL.Name)]
getArgList = map (\(t, n) -> (getType t, getName n))


type Names = Map.Map String Int


-- Takes a name and check if present in names and then checks if the name already there or not.
-- returns updated map and prev. name if any otherwise the name itself. 
-- Can be used to ask if this name is avaible and if so then use it otherwise it returns you 
-- an updated name that can be used.
unikName :: String -> Names -> (String, Names)
unikName name mapping = case Map.lookup name mapping of 
        Nothing -> (name, Map.insert name 1 mapping)
        Just idx -> (name ++ (show idx) , Map.insert name (idx+1) mapping)

type SymbolTable = [(String, ASTL.Operand)]




data CodegenState
  = CodegenState {
    currentBlock :: ASTL.Name                     -- Name of the active block to append to
  , blocks       :: Map.Map ASTL.Name BlockState  -- Blocks for function
  , symtab       :: SymbolTable              -- Function scope symbol table
  , blockCount   :: Int                      -- Count of basic blocks
  , count        :: Word                     -- Count of unnamed instructions a.k.a like function args
  , names        :: Names                    -- Name Supply
  } deriving Show

data BlockState
  = BlockState {
    idx   :: Int                            -- Block index
  , stack :: [ASTL.Named ASTL.Instruction]            -- Stack of instructions Head --> |_| |_| |_| |_|
  , term  :: Maybe (ASTL.Named ASTL.Terminator)       -- Block terminator
  } deriving Show


newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState )

sortBlocks :: [(ASTL.Name, BlockState)] -> [(ASTL.Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)

makeBlock :: (ASTL.Name, BlockState) -> BasicBlock
makeBlock (l, (BlockState _ s t)) = BasicBlock l (reverse s) (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: " ++ (show l)

entryBlockName :: String
entryBlockName = "entry"

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

emptyCodegen :: CodegenState
emptyCodegen = CodegenState (ASTL.Name entryBlockName) Map.empty [] 1 0 Map.empty

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen


------------------------------- Block Operations ------------------------

-- Modifies the current block that is being used using the block name arg.
-- Returns the input bname after successfull updation
setBlock :: ASTL.Name -> Codegen ASTL.Name
setBlock bname = do
  modify $ \s -> s { currentBlock = bname }
  return bname



-- | Alloca Returns an instruction of declaration of 
-- | A var of allocatedType :: Type , numElements , alignment , metadata 
alloca :: ASTL.Type -> Codegen ASTL.Operand
alloca ty = instr $ ASTL.Alloca ty Nothing 0 [] 


-- | Updates the count of unnamed instructions and returns total no. of unnamed instructions
fresh :: Codegen Word
fresh = do
    i <- gets count
    modify $ \s -> s { count = 1 + i }
    return $ i + 1


instr :: ASTL.Instruction -> Codegen (ASTL.Operand)
instr ins = do
    n <- fresh  -- Updated the count of variables/ n=count
    let ref = (ASTL.UnName n)  -- a number for a nameless thing
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





-- | Update the symbol table with the current value and identifier
assign :: String -> ASTL.Operand -> Codegen ()
assign var x = do
  lcls <- gets symtab
  modify $ \s -> s { symtab = [(var, x)] ++ lcls }

getvar :: String -> Codegen ASTL.Operand
getvar var = do
  syms <- gets symtab
  case lookup var syms of
    Just x  -> return x
    Nothing -> error $ "Local variable not in scope: " ++ show var



-- Takes the operand and create a terminator based on that . 
-- Do simply names the instruction 
-- terminator just uplift the value further to Codegen while updating the blockState
ret :: ASTL.Operand -> Codegen (ASTL.Named ASTL.Terminator)
ret val = terminator $ ASTL.Do $ ASTL.Ret (Just val) []


-- Update the terminator of the current active block
terminator :: ASTL.Named ASTL.Terminator -> Codegen (ASTL.Named ASTL.Terminator)
terminator trm = do
  blk <- current
  modifyBlock (blk { term = Just trm })
  return trm


-- Takes a name and gives you an operands
local ::  ASTL.Name -> ASTL.Operand
local = ASTL.LocalReference intL



-- Load And Store operations
store :: ASTL.Operand -> ASTL.Operand -> Codegen ASTL.Operand
store ptr val = instr $ Store False ptr val Nothing 0 []

load :: ASTL.Operand -> Codegen ASTL.Operand
load ptr = instr $ Load False ptr Nothing 0 []