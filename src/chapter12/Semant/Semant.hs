module Semant.Semant where

import qualified Common.Symbol as S
import qualified Common.Temp as Temp
import qualified Semant.Env as E
import qualified Semant.Translate as TL
import qualified Semant.Types as T
import qualified Syntax.Absyn as A

import Control.Monad.State
import Data.List (elemIndex)

type VEnv = S.Table E.EnvEntry
type TEnv = S.Table (TL.Level, T.Ty)

data ExpTy = ExpTy {exptyExp :: TL.Exp, exptyTy :: T.Ty} deriving (Show)

------------------------------------------------------------------
-- SemantState
------------------------------------------------------------------
data SemantState = SS {venv :: VEnv, tenv :: TEnv, level :: TL.Level, tstate :: Temp.TempState}

match :: T.Ty -> T.Ty -> TEnv -> A.Pos -> Either Err.Error ()
match lty rty tenv pos = do
        lty' <- actualTy lty
        rty' <- actualTy rty
        if lty' == rty' || lty' == T.NIL || T.NIL == rty'
                then return ()
                else Err.typeMismatch (show lty) (show rty) pos
    where
        actualTy :: T.Ty -> Either Err.Error T.Ty
        actualTy ty = case ty of
                T.NAME name -> do
                        ty' <- lookty tenv name pos
                        actualTy ty'
                T.ARRAY sym ty' -> T.ARRAY sym <$> actualTy ty'
                T.RECORD sym fields -> T.RECORD sym . zip (map fst fields) <$> mapM (actualTy . snd) fields
                _ -> return ty

lookty :: TEnv -> S.Symbol -> A.Pos -> Either Err.Error T.Ty
lookty tenv typ pos = case S.look tenv typ of
        Nothing -> Err.unknownType typ pos
        Just (_, ty) -> return ty

------------------------------------------------------------------
-- Translate variables
------------------------------------------------------------------
transVar :: MonadThrow m => SemantState -> A.Var -> StateT Temp.TempState m ExpTy
transVar st@(SS venv tenv lev tst) = trvar
    where
        trvar :: A.Var -> StateT Temp.TempState (Either Err.Error) ExpTy
        trvar (A.SimpleVar sym pos) = case S.look venv sym of
                Nothing -> lift $ Err.unknownVariable sym pos
                Just (E.VarEntry acs ty) -> return $ ExpTy (TL.simpleVar (acs, lev)) ty
                Just E.FunEntry{} -> throwString "" sym pos
        trvar (A.FieldVar var sym pos) = do
                ExpTy var' ty <- trvar var
                case ty of
                        T.RECORD _ fs -> case lookup sym fs of
                                Nothing -> lift $ Err.otherError ("field not found" ++ show sym) pos
                                Just ty' -> do
                                        let Just idx = elemIndex sym (map fst fs)
                                        expr <- mkT $ TL.lvalueVar var' (TL.intExp idx)
                                        return $ ExpTy expr ty'
                        _ -> lift $ Err.wrongType "record type" (show ty) pos
        trvar (A.SubscriptVar var exp pos) = do
                ExpTy var' ty <- trvar var
                case ty of
                        T.ARRAY _ ty' -> do
                                ExpTy idx ty'' <- lift $ transExp st exp
                                lift $ (T.INT `match` ty'') tenv pos
                                expr <- mkT $ TL.lvalueVar var' idx
                                return $ ExpTy expr ty'
                        _ -> lift $ Err.wrongType "array type" (show var) pos
