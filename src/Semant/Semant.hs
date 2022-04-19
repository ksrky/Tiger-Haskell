module Semant.Semant where

import qualified Error.Error as Err
import qualified Semant.Env as E
import qualified Semant.Symbol as S
import qualified Semant.Types as T
import qualified Semant.Types.Utils as T
import qualified Syntax.Absyn as A
import qualified Syntax.Absyn.Utils as A

import Control.Monad.State
import Data.Maybe (isJust)
import Error.Error (ErrorKind (TypeNotFound))

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
        if lty' == rty' || T.nil == lty' || T.nil == rty'
                then return ()
                else Err.returnErr_ (Err.TypeMismatch (show lty) (show rty)) pos
    where
        actualTy :: T.Ty -> Either Err.Error T.Ty
        actualTy ty = case ty of
                T.TCon (T.NAME name) -> case S.look tenv name of
                        Just ty' -> actualTy ty'
                        Nothing -> Err.returnErr "" (Err.TypeNotFound name) pos
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
                Just (E.FunEntry{}) -> Err.returnErr_ (Err.UnknownIdentifier sym) pos
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
                                match T.int ty'' tenv pos
                                return $ ExpTy () ty'
                        _ -> Err.returnErr_ (Err.WrongType "array type" (show v)) pos

transExp :: SemantState -> A.Exp -> Either Err.Error ExpTy
transExp st@(SS venv tenv) = trexp
    where
        trexp :: A.Exp -> Either Err.Error ExpTy
        trexp (A.VarExp v) = transVar st v
        trexp A.NilExp = return $ ExpTy () T.nil
        trexp (A.IntExp _) = return $ ExpTy () T.int
        trexp (A.StringExp _) = return $ ExpTy () T.string
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
                        (T.TCon T.INT, T.TCon T.INT) -> case A.opkind op of
                                A.Arith -> True
                                A.Order -> True
                                A.Equal -> True
                        (T.TCon T.STRING, T.TCon T.STRING) -> case A.opkind op of
                                A.Order -> True
                                A.Equal -> True
                                _ -> False
                        (t, t') -> case A.opkind op of
                                A.Equal | t == t' -> True
                                _ -> False
                        then return $ ExpTy () T.int
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
                                T.TCon (T.NAME name) -> case S.look tenv name of
                                        Just ty' -> isRecord ty' -- warning: recursive
                                        Nothing -> Err.returnErr_ (Err.TypeNotFound name) pos
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
                [] -> return $ ExpTy () T.unit
                e : es -> do
                        mapM_ trexp (reverse es)
                        trexp e
        trexp (A.AssignExp v exp pos) = do
                ty <- ty <$> transVar st v
                ty' <- getTy exp
                match ty ty' tenv pos
                return $ ExpTy () T.unit
        trexp (A.IfExp test then' else' pos) = do
                test_ty <- getTy test
                match T.int test_ty tenv pos
                then_ty <- getTy then'
                case else' of
                        Nothing -> do
                                match T.unit then_ty tenv pos
                                return $ ExpTy () T.unit
                        Just else'' -> do
                                else_ty <- getTy else''
                                match then_ty else_ty tenv pos
                                return $ ExpTy () then_ty
        trexp (A.WhileExp test body pos) = do
                test_ty <- getTy test
                match T.int test_ty tenv pos
                body_ty <- getTy body
                match T.unit body_ty tenv pos
                return $ ExpTy () T.unit
        trexp (A.ForExp name _ lo hi body pos) = do
                lo_ty <- getTy lo
                hi_ty <- getTy hi
                match T.int lo_ty tenv pos
                match T.int hi_ty tenv pos
                trexp body
        trexp (A.BreakExp _) = return $ ExpTy () T.unit
        trexp (A.LetExp decs body pos) = do
                st' <- transDecs decs st
                transExp st' body
        trexp (A.ArrayExp typ size init pos) = case S.look tenv typ of
                Nothing -> Err.returnErr_ (Err.TypeNotFound typ) pos
                Just ty -> do
                        ty' <- isArray ty
                        size_ty <- getTy size
                        match T.int size_ty tenv pos
                        init_ty <- getTy init
                        match ty' init_ty tenv pos
                        return $ ExpTy () ty
            where
                isArray :: T.Ty -> Either Err.Error T.Ty
                isArray ty = case ty of
                        T.TCon (T.NAME name) -> case S.look tenv name of
                                Just ty' -> isArray ty' -- warning: recursive
                                Nothing -> Err.returnErr_ (Err.TypeNotFound name) pos
                        T.ARRAY _ ty' -> return ty'
                        _ -> Err.returnErr_ (Err.WrongType "array type" (show ty)) pos

        getTy :: A.Exp -> Either Err.Error T.Ty
        getTy exp = ty <$> trexp exp

type TypeDec = (A.Symbol, A.Pos)
type FunDec = (S.Symbol, [(S.Symbol, T.Ty, Bool)], T.Ty, A.Exp, A.Pos)

transDecs :: [A.Dec] -> SemantState -> Either Err.Error SemantState
transDecs decs = execStateT $ do
        typedecs <- concat <$> mapM regisTypeDec decs
        fundecs <- concat <$> mapM regisFunDec decs
        mapM_ transTypeDec typedecs
        mapM_ transFunDec fundecs
        mapM_ transVarDec decs
    where
        regisTypeDec :: A.Dec -> StateT SemantState (Either Err.Error) [TypeDec]
        regisTypeDec (A.TypeDec name ty pos) = StateT $ \st@(SS venv tenv) -> do
                when (isJust $ S.look tenv name) (Err.returnErr_ (Err.MultipleDeclarations name) pos)
                let tenv' = S.enter tenv name (T.Ref ty) --tmp
                return ([(name, pos)], st{tenv = tenv'})
        regisTypeDec _ = return []
        regisFunDec :: A.Dec -> StateT SemantState (Either Err.Error) [FunDec]
        regisFunDec (A.FunDec name params result body pos) = StateT $ \st@(SS venv tenv) -> do
                when (isJust $ S.look venv name) (Err.returnErr_ (Err.MultipleDeclarations name) pos)
                let resultTy :: Maybe (S.Symbol, A.Pos) -> Either Err.Error T.Ty
                    resultTy result = case result of
                        Just (typ, pos) -> case S.look tenv typ of
                                Nothing -> Err.returnErr_ (Err.TypeNotFound typ) pos
                                Just result_ty -> return result_ty
                        Nothing -> return T.unit
                    transparam :: A.Field -> Either Err.Error (S.Symbol, T.Ty, Bool)
                    transparam (A.Field name esc typ pos) = case S.look tenv typ of
                        Just ty -> return (name, ty, esc)
                        Nothing -> Err.returnErr_ (Err.TypeNotFound typ) pos
                params' <- mapM transparam params
                result_ty <- resultTy result
                let fun = E.FunEntry (map (\(_, x, _) -> x) params') result_ty
                    venv' = S.enter venv name fun
                return ([(name, params', result_ty, body, pos)], st{venv = venv'})
        regisFunDec _ = return []
        transTypeDec :: TypeDec -> StateT SemantState (Either Err.Error) ()
        transTypeDec (name, pos) = StateT $ \st -> do
                st' <- execStateT (transTy (name, pos)) st
                return ((), st')
        transFunDec :: FunDec -> StateT SemantState (Either Err.Error) ()
        transFunDec (name, params', result_ty, body, pos) = StateT $ \st@(SS venv tenv) -> do
                let enterparam :: (S.Symbol, T.Ty, Bool) -> State SemantState ()
                    enterparam (name, ty, esc) = do
                        st@(SS venv _) <- get
                        let venv' = S.enter venv name (E.VarEntry ty)
                        put st{venv = venv'}
                        return ()
                    st' = execState (mapM enterparam params') st
                ty <- ty <$> transExp st' body
                match result_ty ty tenv pos
                return ((), st)
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

transTy :: (A.Symbol, A.Pos) -> StateT SemantState (Either Err.Error) T.Ty
transTy (name, pos) = StateT $ \st@(SS _ tenv) -> do
        (ty, tenv') <- transTy' (name, pos) tenv
        return (ty, st{tenv = S.enter tenv' name ty})
    where
        transTy' :: (A.Symbol, A.Pos) -> TEnv -> Either Err.Error (T.Ty, TEnv)
        transTy' (name, pos) tenv = case S.look tenv name of
                Just (T.Ref (A.NameTy typ p)) -> do
                        (ty, tenv') <- transTyCon [] (typ, pos) tenv
                        return (ty, tenv')
                Just (T.Ref (A.RecordTy fields)) -> do
                        let transfield :: A.Field -> Either Err.Error (S.Symbol, T.Ty)
                            transfield (A.Field name _ typ p) = do
                                (ty, _) <- transTyCon [] (typ, p) tenv
                                return (name, ty)
                        fs_ty <- mapM transfield fields
                        return (T.RECORD name fs_ty, S.enter tenv name (T.RECORD name fs_ty))
                Just (T.Ref (A.ArrayTy typ p)) -> do
                        (ty, tenv') <- transTyCon [] (typ, p) tenv
                        return (T.ARRAY name ty, S.enter tenv' typ ty)
                Just ty -> return (ty, tenv)
                _ -> Err.returnErr_ (Err.TypeNotFound name) pos
        transTyCon :: [A.Symbol] -> (A.Symbol, A.Pos) -> TEnv -> Either Err.Error (T.Ty, TEnv)
        transTyCon checked (name, pos) tenv = do
                when (name `elem` checked) (Err.returnErr "" (Err.CyclicDefinition name) pos)
                case S.look tenv name of
                        Just (T.Ref (A.NameTy typ p)) -> do
                                (ty, tenv') <- transTyCon (checked ++ [name]) (typ, p) tenv
                                return (ty, S.enter tenv' typ ty)
                        Just (T.Ref _) -> return (T.name name, tenv)
                        Just (T.TCon tycon) -> return (T.TCon tycon, tenv)
                        Just ty -> return (ty, tenv)
                        Nothing -> Err.returnErr_ (Err.TypeNotFound name) pos

{-transTy :: (A.Symbol, A.Pos) -> StateT SemantState (Either Err.Error) T.Ty
transTy (name, pos) = StateT $ \st@(SS _ tenv) -> do
        (ty, tenv') <- transTy' [] (name, pos) tenv
        return (ty, st{tenv = S.enter tenv' name ty})
    where
        transTy' :: [A.Symbol] -> (A.Symbol, A.Pos) -> TEnv -> Either Err.Error (T.Ty, TEnv)
        transTy' checked (name, pos) tenv = case S.look tenv name of
                Just (T.Ref (A.NameTy typ p)) -> do
                        when (typ `elem` checked) (Err.returnErr "" (Err.CyclicDefinition typ) pos)
                        (ty, tenv') <- transTy' (checked ++ [name]) (typ, p) tenv
                        return (ty, S.enter tenv' typ ty)
                Just (T.Ref (A.RecordTy fields)) -> do
                        let tenv' = S.enter tenv name (T.name name)
                            transfield :: A.Field -> Either Err.Error (S.Symbol, T.Con)
                            transfield (A.Field name _ typ p) = do
                                (ty, _) <- transTy' [] (typ, p) tenv'
                                return (name, ty)
                        fs_ty <- mapM transfield fields
                        return (T.RECORD name fs_ty, tenv')
                Just (T.Ref (A.ArrayTy typ p)) -> do
                        let tenv' = S.enter tenv name (T.name name)
                        (ty, tenv'') <- transTy' [] (typ, p) tenv'
                        return (T.ARRAY name ty, tenv'')
                Just ty -> return (ty, tenv)
                _ -> Err.returnErr_ (Err.TypeNotFound name) pos-}

transProg :: A.Exp -> Either Err.Error ()
transProg exp = do
        transExp (SS E.baseVEnv E.baseTEnv) exp
        return ()
