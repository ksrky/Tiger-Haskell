module Semant.Semant where

import qualified Semant.Env as E
import qualified Semant.Error as Err
import qualified Semant.Symbol as S
import qualified Semant.Types as T
import qualified Syntax.Absyn as A
import qualified Syntax.Absyn.Utils as A

import Control.Monad.State
import Data.Maybe (isJust)

type VEnv = S.Table E.EnvEntry
type TEnv = S.Table T.Ty

data ExpTy = ExpTy {exp :: (), ty :: T.Ty}

instance Show ExpTy where
        show (ExpTy _ ty) = show ty

data SemantState = SS {venv :: VEnv, tenv :: TEnv}

match :: T.Ty -> T.Ty -> TEnv -> A.Pos -> Either Err.Error ()
match lty rty tenv pos = do
        lty' <- actualTy lty
        rty' <- actualTy rty
        if lty' == rty' || T.NIL == lty' || T.NIL == rty'
                then return ()
                else Err.returnErr_ (Err.TypeMismatch (show lty) (show rty)) pos
    where
        actualTy :: T.Ty -> Either Err.Error T.Ty
        actualTy ty = case ty of
                T.NAME _ (Just sym) -> case S.look tenv sym of
                        Just ty' -> actualTy ty'
                        Nothing -> Err.returnErr_ (Err.TypeNotFound sym) pos
                T.ARRAY sym ty' -> T.ARRAY sym <$> actualTy ty'
                T.RECORD sym fields -> T.RECORD sym . zip (map fst fields) <$> mapM (actualTy . snd) fields
                _ -> return ty

transVar :: SemantState -> A.Var -> Either Err.Error ExpTy
transVar st@(SS venv tenv) = trvar
    where
        trvar :: A.Var -> Either Err.Error ExpTy
        trvar (A.SimpleVar sym pos) = case S.look venv sym of
                Nothing -> Err.returnErr_ (Err.UnknownIdentifier sym) pos
                Just (E.VarEntry ty) -> return $ ExpTy () ty
                Just E.FunEntry{} -> Err.returnErr_ (Err.UnknownIdentifier sym) pos
        trvar (A.FieldVar v sym pos) = do
                ty <- ty <$> trvar v
                case ty of
                        T.RECORD _ fs -> case lookup sym fs of
                                Nothing -> Err.returnErr_ (Err.RecordFieldNotFound sym) pos
                                Just ty' -> return $ ExpTy () ty'
                        _ -> Err.returnErr_ (Err.WrongType "record type" (show ty)) pos
        trvar (A.SubscriptVar v exp pos) = do
                t <- ty <$> trvar v
                case t of
                        T.ARRAY _ ty' -> do
                                ty'' <- ty <$> transExp st exp
                                match T.INT ty'' tenv pos
                                return $ ExpTy () ty'
                        _ -> Err.returnErr_ (Err.WrongType "array type" (show v)) pos

transExp :: SemantState -> A.Exp -> Either Err.Error ExpTy
transExp st@(SS venv tenv) = trexp
    where
        trexp :: A.Exp -> Either Err.Error ExpTy
        trexp (A.VarExp v) = transVar st v
        trexp A.NilExp = return $ ExpTy () T.NIL
        trexp (A.IntExp _) = return $ ExpTy () T.INT
        trexp (A.StringExp _) = return $ ExpTy () T.STRING
        trexp (A.CallExp fun args pos) = case S.look venv fun of
                Nothing -> Err.returnErr_ (Err.UnknownIdentifier fun) pos
                Just (E.VarEntry _) -> Err.returnErr_ (Err.UnknownIdentifier fun) pos
                Just (E.FunEntry fmls res) -> do
                        checkformals args fmls
                        return $ ExpTy () res
                    where
                        checkformals :: [A.Exp] -> [T.Ty] -> Either Err.Error ()
                        checkformals [] [] = return ()
                        checkformals (e : es) (t : ts) = do
                                ty <- getTy e
                                match t ty tenv pos
                                checkformals es ts
                                return ()
                        checkformals _ _ = Err.returnErr "wrong number of arguments" Err.SizeMismatch pos
        trexp (A.OpExp left op right pos) = do
                lty <- getTy left
                rty <- getTy right
                if case (lty, rty) of
                        (T.INT, T.INT) -> case A.opkind op of
                                A.Arith -> True
                                A.Order -> True
                                A.Equal -> True
                        (T.STRING, T.STRING) -> case A.opkind op of
                                A.Order -> True
                                A.Equal -> True
                                _ -> False
                        (t, t') -> case A.opkind op of
                                A.Equal | t == t' -> True
                                _ -> False
                        then return $ ExpTy () T.INT
                        else Err.returnErr_ (Err.InvalidComparison (show lty) (show rty) (show op)) pos
        trexp (A.RecordExp fields typ pos) = case S.look tenv typ of
                Nothing -> Err.returnErr_ (Err.TypeNotFound typ) pos
                Just ty' -> do
                        fs_ty <- isRecord ty'
                        checkrecord (map (\(x, y, z) -> (x, (y, z))) fields) fs_ty
                        return $ ExpTy () ty'
                    where
                        isRecord :: T.Ty -> Either Err.Error [(S.Symbol, T.Ty)]
                        isRecord ty = case ty of
                                T.NAME _ (Just sym) -> case S.look tenv sym of
                                        Just ty' -> isRecord ty' -- warning: recursive
                                        Nothing -> Err.returnErr_ (Err.TypeNotFound sym) pos
                                T.RECORD _ fields -> return fields
                                _ -> Err.returnErr_ (Err.WrongType "record type" (show ty)) pos
                        checkrecord :: [(A.Symbol, (A.Exp, A.Pos))] -> [(S.Symbol, T.Ty)] -> Either Err.Error ()
                        checkrecord _ [] = return ()
                        checkrecord fs ((s, t) : sts) = case lookup s fs of
                                Just (e, p) -> do
                                        ty <- getTy e
                                        match t ty tenv pos
                                        checkrecord fs sts
                                Nothing -> Err.returnErr_ (Err.RecordFieldNotFound s) pos
        trexp (A.SeqExp seqexp pos) = case reverse seqexp of
                [] -> return $ ExpTy () T.UNIT
                e : es -> do
                        mapM_ trexp (reverse es)
                        trexp e
        trexp (A.AssignExp v exp pos) = do
                ty <- ty <$> transVar st v
                ty' <- getTy exp
                match ty ty' tenv pos
                return $ ExpTy () T.UNIT
        trexp (A.IfExp test then' else' pos) = do
                test_ty <- getTy test
                match T.INT test_ty tenv pos
                then_ty <- getTy then'
                case else' of
                        Nothing -> do
                                match T.UNIT then_ty tenv pos
                                return $ ExpTy () T.UNIT
                        Just else'' -> do
                                else_ty <- getTy else''
                                match then_ty else_ty tenv pos
                                return $ ExpTy () then_ty
        trexp (A.WhileExp test body pos) = do
                test_ty <- getTy test
                match T.INT test_ty tenv pos
                body_ty <- getTy body
                match T.UNIT body_ty tenv pos
                return $ ExpTy () T.UNIT
        trexp (A.ForExp name _ lo hi body pos) = do
                lo_ty <- getTy lo
                hi_ty <- getTy hi
                match T.INT lo_ty tenv pos
                match T.INT hi_ty tenv pos
                trexp body
        trexp (A.BreakExp _) = return $ ExpTy () T.UNIT
        trexp (A.LetExp decs body pos) = do
                st' <- transDecs decs st
                transExp st' body
        trexp (A.ArrayExp typ size init pos) = case S.look tenv typ of
                Nothing -> Err.returnErr_ (Err.TypeNotFound typ) pos
                Just ty -> do
                        ty' <- isArray ty
                        size_ty <- getTy size
                        match T.INT size_ty tenv pos
                        init_ty <- getTy init
                        match ty' init_ty tenv pos
                        return $ ExpTy () ty
            where
                isArray :: T.Ty -> Either Err.Error T.Ty
                isArray ty = case ty of
                        T.NAME _ (Just sym) -> case S.look tenv sym of
                                Just ty' -> isArray ty' -- warning: recursive
                                Nothing -> Err.returnErr_ (Err.TypeNotFound sym) pos
                        T.ARRAY _ ty' -> return ty'
                        _ -> Err.returnErr_ (Err.WrongType "array type" (show ty)) pos

        getTy :: A.Exp -> Either Err.Error T.Ty
        getTy exp = ty <$> trexp exp

transDecs :: [A.Dec] -> SemantState -> Either Err.Error SemantState
transDecs decs = execStateT $ do
        mapM_ transTypeDec decs
        mapM_ transFunDec decs
        mapM_ transVarDec decs
    where
        transTypeDec :: A.Dec -> StateT SemantState (Either Err.Error) ()
        transTypeDec (A.TypeDec name ty pos) = StateT $ \st@(SS venv tenv) -> do
                when (isJust $ S.look tenv name) (Err.returnErr_ (Err.MultipleDeclarations name) pos)
                ty' <- transTy name ty tenv
                let tenv' = S.enter tenv name ty'
                return ((), st{tenv = tenv'})
        transTypeDec _ = return ()
        transFunDec :: A.Dec -> StateT SemantState (Either Err.Error) ()
        transFunDec (A.FunDec name params result body pos) = StateT $ \st@(SS venv tenv) -> do
                when (isJust $ S.look venv name) (Err.returnErr_ (Err.MultipleDeclarations name) pos)
                let resultTy :: Maybe (S.Symbol, A.Pos) -> Either Err.Error T.Ty
                    resultTy result = case result of
                        Just (typ, pos) -> case S.look tenv typ of
                                Nothing -> Err.returnErr_ (Err.TypeNotFound typ) pos
                                Just result_ty -> return result_ty
                        Nothing -> return T.UNIT
                    transparam :: A.Field -> Either Err.Error (S.Symbol, T.Ty, Bool)
                    transparam (A.Field name esc typ pos) = case S.look tenv typ of
                        Just t -> return (name, t, esc)
                        Nothing -> Err.returnErr_ (Err.TypeNotFound typ) pos
                    enterparam :: (S.Symbol, T.Ty, Bool) -> State SemantState ()
                    enterparam (name, ty, esc) = do
                        st@(SS venv _) <- get
                        let venv' = S.enter venv name (E.VarEntry ty)
                        put st{venv = venv'}
                        return ()
                params' <- mapM transparam params
                result_ty <- resultTy result
                let fun = E.FunEntry (map (\(_, x, _) -> x) params') result_ty
                    venv' = S.enter venv name fun
                    st' = execState (mapM enterparam params') st{venv = venv'}
                ty <- ty <$> transExp st' body
                match result_ty ty tenv pos
                return ((), st{venv = venv'})
        transFunDec _ = return ()
        transVarDec :: A.Dec -> StateT SemantState (Either Err.Error) ()
        transVarDec (A.VarDec name esc mtyp init pos) = StateT $ \st@(SS venv tenv) -> case mtyp of
                Just (typ, p) -> case S.look tenv typ of
                        Nothing -> Err.returnErr_ (Err.TypeNotFound typ) pos
                        Just t -> do
                                ty <- ty <$> transExp st init
                                match t ty tenv pos
                                let venv' = S.enter venv name (E.VarEntry ty)
                                return ((), st{venv = venv'})
                Nothing -> do
                        ty <- ty <$> transExp st init
                        let venv' = S.enter venv name (E.VarEntry ty)
                        return ((), st{venv = venv'})
        transVarDec _ = return ()

transTy :: A.Symbol -> A.Ty -> TEnv -> Either Err.Error T.Ty
transTy name (A.NameTy typ pos) tenv = case S.look tenv typ of
        Just ty -> return ty
        Nothing -> Err.returnErr_ (Err.TypeNotFound typ) pos
transTy name (A.RecordTy fields) tenv = do
        let transfield :: A.Field -> Either Err.Error (S.Symbol, T.Ty)
            transfield (A.Field name _ typ pos) = case S.look tenv typ of
                Just ty -> return (name, ty)
                Nothing -> Err.returnErr_ (Err.TypeNotFound typ) pos
        fs_ty <- mapM transfield fields
        return $ T.RECORD name fs_ty
transTy name (A.ArrayTy typ pos) tenv = case S.look tenv typ of
        Just ty -> return $ T.ARRAY name ty
        Nothing -> Err.returnErr_ (Err.TypeNotFound typ) pos

transProg :: A.Exp -> Either Err.Error ()
transProg exp = do
        transExp (SS E.baseVEnv E.baseTEnv) exp
        return ()
