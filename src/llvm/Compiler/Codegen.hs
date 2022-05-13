{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Compiler.Codegen where

import Control.Monad.State
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Short as B.Short
import Data.Char (ord)
import Data.Function
import Data.List
import qualified Data.Map.Strict as M

import LLVM.AST as AST
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FloatingPointPredicate as FP
import LLVM.AST.Global as G
import qualified LLVM.AST.Global
import qualified LLVM.AST.Linkage as L
import LLVM.AST.Type
import qualified LLVM.AST.Typed

-------------------------------------------------------------------------------
-- Module Level
-------------------------------------------------------------------------------

newtype LLVM a = LLVM (State AST.Module a)
        deriving (Functor, Applicative, Monad, MonadState AST.Module)

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM mod (LLVM m) = execState m mod

emptyModule :: String -> AST.Module
emptyModule label = defaultModule{moduleName = B.Short.toShort $ C.pack label}

addDefn :: Definition -> LLVM ()
addDefn d = do
        defs <- gets moduleDefinitions
        modify $ \s -> s{moduleDefinitions = defs ++ [d]}

function :: String -> [(Type, Name)] -> Type -> [BasicBlock] -> LLVM Operand
function label argtys retty body = do
        addDefn $
                GlobalDefinition
                        functionDefaults
                                { name = mkName label
                                , parameters = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
                                , returnType = retty
                                , basicBlocks = body
                                }
        let funty = ptr $ FunctionType retty (fst <$> argtys) False
        return $ ConstantOperand $ C.GlobalReference funty (mkName label)

extern :: String -> [(Type, Name)] -> Type -> LLVM Operand
extern nm argtys retty = do
        addDefn $
                GlobalDefinition
                        functionDefaults
                                { name = mkName nm
                                , linkage = L.External
                                , parameters = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
                                , returnType = retty
                                }
        let funty = ptr $ FunctionType retty (fst <$> argtys) False
        return $ ConstantOperand $ C.GlobalReference funty (mkName nm)

global :: String -> Type -> C.Constant -> LLVM Operand
global nm ty initVal = do
        addDefn $
                GlobalDefinition
                        globalVariableDefaults
                                { name = mkName nm
                                , LLVM.AST.Global.type' = ty
                                , linkage = L.External
                                , initializer = Just initVal
                                }
        return $ ConstantOperand $ C.GlobalReference (ptr ty) (mkName nm)

externVarArgs :: Name -> [Type] -> Type -> LLVM Operand
externVarArgs nm argtys retty = do
        addDefn $
                GlobalDefinition
                        functionDefaults
                                { name = nm
                                , linkage = L.External
                                , parameters = ([Parameter ty (mkName "") [] | ty <- argtys], True)
                                , returnType = retty
                                }
        let funty = ptr $ FunctionType retty argtys True
        return $ ConstantOperand $ C.GlobalReference funty nm

globalStringPtr :: String -> Name -> LLVM C.Constant
globalStringPtr str nm = do
        let asciiVals = map (fromIntegral . ord) str
            llvmVals = map (C.Int 8) (asciiVals ++ [0]) -- append null terminator
            char = IntegerType 8
            charStar = ptr char
            charArray = C.Array char llvmVals
            ty = LLVM.AST.Typed.typeOf charArray
        addDefn $
                GlobalDefinition
                        globalVariableDefaults
                                { name = nm
                                , LLVM.AST.Global.type' = ty
                                , linkage = L.External
                                , isConstant = True
                                , initializer = Just charArray
                                , unnamedAddr = Just GlobalAddr
                                }
        return $
                C.GetElementPtr
                        True
                        (C.GlobalReference (ptr ty) nm)
                        [C.Int 32 0, C.Int 32 0]

-------------------------------------------------------------------------------
-- Names
-------------------------------------------------------------------------------

type Names = M.Map String Int

uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns =
        case M.lookup nm ns of
                Nothing -> (nm, M.insert nm 1 ns)
                Just ix -> (nm ++ show ix, M.insert nm (ix + 1) ns)

-------------------------------------------------------------------------------
-- Codegen State
-------------------------------------------------------------------------------

type SymbolTable = [(String, Operand)]

data CodegenState = CodegenState
        { currentBlock :: Name -- Name of the active block to append to
        , blocks :: M.Map Name BlockState -- Blocks for function
        , symtab :: SymbolTable -- Function scope symbol table
        , blockCount :: Int -- Count of basic blocks
        , count :: Word -- Count of unnamed instructions
        , names :: Names -- Name Supply
        }
        deriving (Show)

data BlockState = BlockState
        { idx :: Int -- Block index
        , stack :: [Named Instruction] -- Stack of instructions
        , term :: Maybe (Named Terminator) -- Block terminator
        }
        deriving (Show)

-------------------------------------------------------------------------------
-- Codegen Operations
-------------------------------------------------------------------------------

newtype Codegen a = Codegen {runCodegen :: State CodegenState a}
        deriving (Functor, Applicative, Monad, MonadState CodegenState)

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ M.toList (blocks m)

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, BlockState _ s t) = BasicBlock l (reverse s) (maketerm t)
    where
        maketerm (Just x) = x
        maketerm Nothing = error $ "Block has no terminator: " ++ show l

entryBlockName :: String
entryBlockName = "entry"

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

emptyCodegen :: CodegenState
emptyCodegen = CodegenState (mkName entryBlockName) M.empty [] 1 0 M.empty

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen

evalCodegen :: Codegen a -> a
evalCodegen m = evalState (runCodegen m) emptyCodegen

fresh :: Codegen Word
fresh = do
        i <- gets count
        modify $ \s -> s{count = 1 + i}
        return $ i + 1

instr :: Instruction -> Codegen Operand
instr ins = do
        n <- fresh
        let ref = UnName n
        blk <- current
        let i = stack blk
        modifyBlock (blk{stack = ref := ins : i})
        return $ local ref

instr' :: Instruction -> Codegen ()
instr' ins = do
        blk <- current
        let i = stack blk
        modifyBlock (blk{stack = Do ins : i})
        return ()

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
        blk <- current
        modifyBlock (blk{term = Just trm})
        return trm

-------------------------------------------------------------------------------
-- Block Stack
-------------------------------------------------------------------------------

entry :: Codegen Name
entry = gets currentBlock

addBlock :: String -> Codegen Name
addBlock bname = do
        bls <- gets blocks
        ix <- gets blockCount
        nms <- gets names

        let new = emptyBlock ix
            (qname, supply) = uniqueName bname nms

        modify $ \s ->
                s
                        { blocks = M.insert (mkName qname) new bls
                        , blockCount = ix + 1
                        , names = supply
                        }
        return (mkName qname)

setBlock :: Name -> Codegen Name
setBlock bname = do
        modify $ \s -> s{currentBlock = bname}
        return bname

getBlock :: Codegen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
        active <- gets currentBlock
        modify $ \s -> s{blocks = M.insert active new (blocks s)}

current :: Codegen BlockState
current = do
        c <- gets currentBlock
        blks <- gets blocks
        case M.lookup c blks of
                Just x -> return x
                Nothing -> error $ "No such block: " ++ show c

-------------------------------------------------------------------------------
-- Symbol Table
-------------------------------------------------------------------------------

assign :: String -> Operand -> Codegen ()
assign var x = do
        lcls <- gets symtab
        modify $ \s -> s{symtab = (var, x) : lcls}

getvar :: String -> Codegen Operand
getvar var = do
        syms <- gets symtab
        case lookup var syms of
                Just x -> return x
                Nothing -> error $ "Local variable not in scope: " ++ show var

-------------------------------------------------------------------------------
-- References
-------------------------------------------------------------------------------

local :: Name -> Operand
local = LocalReference double

externf :: Type -> Name -> Operand
externf ty = ConstantOperand . C.GlobalReference ty

-- Arithmetic and Constants
fadd :: Operand -> Operand -> Codegen Operand
fadd a b = instr $ FAdd noFastMathFlags a b []

fsub :: Operand -> Operand -> Codegen Operand
fsub a b = instr $ FSub noFastMathFlags a b []

fmul :: Operand -> Operand -> Codegen Operand
fmul a b = instr $ FMul noFastMathFlags a b []

fdiv :: Operand -> Operand -> Codegen Operand
fdiv a b = instr $ FDiv noFastMathFlags a b []

fcmp :: FP.FloatingPointPredicate -> Operand -> Operand -> Codegen Operand
fcmp cond a b = instr $ FCmp cond a b []

cons :: C.Constant -> Operand
cons = ConstantOperand

uitofp :: Type -> Operand -> Codegen Operand
uitofp ty a = instr $ UIToFP a ty []

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

-- Effects
call :: Operand -> [Operand] -> Codegen Operand
call fn args = instr $ Call Nothing CC.C [] (Right fn) (toArgs args) [] []

alloca :: Type -> Codegen Operand
alloca ty = instr $ Alloca ty Nothing 0 []

store :: Operand -> Operand -> Codegen ()
store ptr val = instr' $ Store False ptr val Nothing 0 []

load :: Operand -> Codegen Operand
load ptr = instr $ Load False ptr Nothing 0 []

-- Control Flow
br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []
