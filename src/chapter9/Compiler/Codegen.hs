module Compiler.Codegen where

import qualified Common.Temp as Temp
import qualified Compiler.Assem as A
import qualified Frame.Frame as Frame
import qualified IR.Tree as T

import Control.Monad.State

data CodegenState = CodegenState
        { ilist :: [A.Instr]
        , temps :: Temp.Temp
        }

codegen :: Frame.FrameBase f => f -> T.Stm -> State CodegenState [A.Instr]
codegen frame stm = do
        munchStm stm
        ilist <- gets ilist
        return $ reverse ilist

newTemp :: State CodegenState Temp.Temp
newTemp = state (\cgn@CodegenState{temps = t} -> (t, cgn{temps = t + 1}))

emit :: A.Instr -> State CodegenState ()
emit inst = state $ \cgn -> ((), cgn{ilist = inst : ilist cgn})

munchStm :: T.Stm -> State CodegenState ()
munchStm (T.SEQ a b) = munchStm a >> munchStm b
munchStm (T.MOVE (T.MEM (T.BINOP T.PLUS e1 (T.CONST i))) e2) = do
        e1' <- munchExp e1
        e2' <- munchExp e2
        emit
                ( A.OPER
                        { A.assem = "STORE M[`s0+" ++ show i ++ "] <- `s1\n"
                        , A.src = [e1', e2']
                        , A.dst = []
                        , A.jump = Nothing
                        }
                )
munchStm (T.MOVE (T.MEM (T.BINOP T.PLUS (T.CONST i) e1)) e2) = do
        e1' <- munchExp e1
        e2' <- munchExp e2
        emit
                ( A.OPER
                        { A.assem = "STORE M[`s0+" ++ show i ++ "] <- `s1\n"
                        , A.src = [e1', e2']
                        , A.dst = []
                        , A.jump = Nothing
                        }
                )
munchStm (T.MOVE (T.MEM e1) (T.MEM e2)) = do
        e1' <- munchExp e1
        e2' <- munchExp e2
        emit
                ( A.OPER
                        { A.assem = "MOVE M[`s0] <- M[`s1]\n"
                        , A.src = [e1', e2']
                        , A.dst = []
                        , A.jump = Nothing
                        }
                )
munchStm (T.MOVE (T.MEM (T.CONST i)) e2) = do
        e2' <- munchExp e2
        emit
                ( A.OPER
                        { A.assem = "STORE M[r0+" ++ show i ++ "] <- `s1\n"
                        , A.src = [e2']
                        , A.dst = []
                        , A.jump = Nothing
                        }
                )
munchStm (T.MOVE (T.MEM e1) e2) = do
        e1' <- munchExp e1
        e2' <- munchExp e2
        emit
                ( A.OPER
                        { A.assem = "STORE M[`s0] <- M[`s1]\n"
                        , A.src = [e1', e2']
                        , A.dst = []
                        , A.jump = Nothing
                        }
                )
munchStm (T.MOVE (T.TEMP i) e2) = do
        e2' <- munchExp e2
        emit
                ( A.OPER
                        { A.assem = "ADD    `d0 <- `s0 + r0\n"
                        , A.src = [e2']
                        , A.dst = [i]
                        , A.jump = Nothing
                        }
                )
munchStm (T.LABEL lab) = emit A.LABEL{A.assem = lab ++ ":\n", A.lab = lab}
munchStm (T.EXP (T.CALL e args)) = do
        e' <- munchExp e
        args' <- forM args munchExp
        t <- newTemp
        emit
                ( A.OPER
                        { A.assem = "CALL `s0\n"
                        , A.src = e' : args'
                        , A.dst = [t]
                        , A.jump = Nothing
                        }
                )
munchStm _ = undefined

result :: (Temp.Temp -> State CodegenState ()) -> State CodegenState Temp.Temp
result gen = do
        t <- newTemp
        gen t
        return t

munchExp :: T.Exp -> State CodegenState Temp.Temp
munchExp (T.MEM (T.BINOP T.PLUS e1 (T.CONST i))) = do
        e1' <- munchExp e1
        result $ \r ->
                emit
                        ( A.OPER
                                { A.assem = "LOAD `d0 <- M[`s0+" ++ show i ++ "]\n"
                                , A.src = [e1']
                                , A.dst = [r]
                                , A.jump = Nothing
                                }
                        )
munchExp (T.MEM (T.BINOP T.PLUS (T.CONST i) e1)) = do
        e1' <- munchExp e1
        result $ \r ->
                emit
                        ( A.OPER
                                { A.assem = "LOAD `d0 <- M[`s0+" ++ show i ++ "]\n"
                                , A.src = [e1']
                                , A.dst = [r]
                                , A.jump = Nothing
                                }
                        )
munchExp (T.MEM (T.CONST i)) = do
        result $ \r ->
                emit
                        ( A.OPER
                                { A.assem = "LOAD `d0 <- M[r0+" ++ show i ++ "]\n"
                                , A.src = []
                                , A.dst = [r]
                                , A.jump = Nothing
                                }
                        )
munchExp (T.MEM e1) = do
        e1' <- munchExp e1
        result $ \r ->
                emit
                        ( A.OPER
                                { A.assem = "LOAD `d0 <- M[`s0+0]\n"
                                , A.src = [e1']
                                , A.dst = [r]
                                , A.jump = Nothing
                                }
                        )
munchExp (T.BINOP T.PLUS e1 (T.CONST i)) = do
        e1' <- munchExp e1
        result $ \r ->
                emit
                        ( A.OPER
                                { A.assem = "ADDI `d0 <- `s0+" ++ show i ++ "\n"
                                , A.src = [e1']
                                , A.dst = [r]
                                , A.jump = Nothing
                                }
                        )
munchExp (T.BINOP T.PLUS (T.CONST i) e1) = do
        e1' <- munchExp e1
        result $ \r ->
                emit
                        ( A.OPER
                                { A.assem = "ADDI `d0 <- `s0+" ++ show i ++ "\n"
                                , A.src = [e1']
                                , A.dst = [r]
                                , A.jump = Nothing
                                }
                        )
munchExp (T.CONST i) = do
        result $ \r ->
                emit
                        ( A.OPER
                                { A.assem = "ADDI `d0 <- r0+" ++ show i ++ "\n"
                                , A.src = []
                                , A.dst = [r]
                                , A.jump = Nothing
                                }
                        )
munchExp (T.BINOP T.PLUS e1 e2) = do
        e1' <- munchExp e1
        e2' <- munchExp e2
        result $ \r ->
                emit
                        ( A.OPER
                                { A.assem = "ADD    `d0 <- `s0+`s1\n"
                                , A.src = [e1', e2']
                                , A.dst = [r]
                                , A.jump = Nothing
                                }
                        )
munchExp (T.TEMP t) = return t
munchExp _ = undefined
