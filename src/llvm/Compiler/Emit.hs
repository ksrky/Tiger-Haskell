{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Compiler.Emit where

import qualified Common.Symbol as S
import qualified Compiler.Codegen as C
import qualified Syntax.Absyn as A

import Control.Monad.State
import Data.Functor.Identity
import qualified Data.Map.Strict as M
import Data.Text

import LLVM.AST hiding (function, value)
import LLVM.AST.Type as AST
import LLVM.Pretty

import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad

type LLVMBuilder = IRBuilderT (ModuleBuilderT Identity)

type SymbolTable = S.Table Operand

compile :: A.Exp -> Text
compile exps = ppllvm $
        buildModule "main" $ mdo
                form <- globalStringPtr "%d\n" "putNumForm"
                printf <- externVarArgs "printf" [ptr i8] i32
                function "main" [] i32 $ \[] -> mdo
                        entry <- block `named` "entry"
                        r <- toOperand exps `evalStateT` M.empty
                        call printf [(ConstantOperand form, []), (r, [])]
                        ret (int32 0)

class LLVMOperand a where
        toOperand :: a -> StateT SymbolTable LLVMBuilder Operand

instance LLVMOperand Integer where
        toOperand n = return (int32 n)

instance LLVMOperand A.Var where
        toOperand (A.SimpleVar s _) = do
                env <- get
                case M.lookup s env of
                        Just oper -> return oper
                        Nothing -> error $ "Unknown variable: " ++ n

instance LLVMOperand A.Exp where
        toOperand (A.VarExp v) = toOperand v
        toOperand A.NilExp = undefined
        toOperand (A.IntExp i) = toOperand i
        toOperand (A.String s _) = undefined
        toOperand (A.UnOp A.Minus e) = binop sub (A.Int 0) e
        toOperand (A.UnOp _ _) = undefined
        toOperand (A.BinOp A.Plus l r) = binop add l r
        toOperand (A.BinOp A.Minus l r) = binop sub l r
        toOperand (A.BinOp A.Times l r) = binop mul l r
        toOperand (A.BinOp A.Divide l r) = binop sdiv l r
        toOperand (A.Assign n e) = do
                e' <- toOperand e
                env <- get
                put $ M.insert n e' env
                return e'

binop :: (Operand -> Operand -> StateT SymbolTable LLVMBuilder Operand) -> A.Expr -> A.Expr -> StateT SymbolTable LLVMBuilder Operand
binop f l r = do
        l' <- toOperand l
        r' <- toOperand r
        f l' r'
