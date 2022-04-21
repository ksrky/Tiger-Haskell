module Semant.Semant where

import qualified Semant.Env as E
import qualified Semant.Error as Err
import qualified Semant.Symbol as S
import qualified Semant.Translate as TL
import qualified Semant.Types as T
import qualified Syntax.Absyn as A
import qualified Syntax.Absyn.Utils as A
import qualified Temp.Temp as Temp

import Control.Monad.State
import Data.Maybe (isJust)

type VEnv = S.Table E.EnvEntry
type TEnv = S.Table T.Ty

data ExpTy = ExpTy {exp :: (), ty :: T.Ty}

instance Show ExpTy where
        show (ExpTy _ ty) = show ty

data SemantState = SS {venv :: VEnv, tenv :: TEnv, level :: TL.Level, tstate :: Temp.TempState}

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
                T.NAME name -> case S.look tenv name of
                        Just ty' -> actualTy ty'
                        Nothing -> Err.returnErr "" (Err.TypeNotFound name) pos
                T.ARRAY sym ty' -> T.ARRAY sym <$> actualTy ty'
                T.RECORD sym fields -> T.RECORD sym . zip (map fst fields) <$> mapM (actualTy . snd) fields
                _ -> return ty

transVar :: SemantState -> A.Var -> Either Err.Error ExpTy
transVar st@(SS venv tenv _ _) = trvar
    where
        trvar :: A.Var -> Either Err.Error ExpTy
        trvar (A.SimpleVar sym pos) = case S.look venv sym of
                Nothing -> Err.returnErr_ (Err.UnknownIdentifier sym) pos
                Just (E.VarEntry _ ty) -> return $ ExpTy () ty
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
                                match T.INT ty'' tenv pos
                                return $ ExpTy () ty'
                        _ -> Err.returnErr_ (Err.WrongType "array type" (show v)) pos

transExp :: SemantState -> A.Exp -> Either Err.Error ExpTy
transExp st@(SS venv tenv _ _) = trexp
    where
        trexp :: A.Exp -> Either Err.Error ExpTy
        trexp (A.VarExp v) = transVar st v
        trexp A.NilExp = return $ ExpTy () T.NIL
        trexp (A.IntExp _) = return $ ExpTy () T.INT
        trexp (A.StringExp _) = return $ ExpTy () T.STRING
        trexp (A.CallExp fun args pos) = case S.look venv fun of
                Nothing -> Err.returnErr_ (Err.UnknownIdentifier fun) pos
                Just (E.VarEntry _ _) -> Err.returnErr_ (Err.UnknownIdentifier fun) pos
                Just (E.FunEntry _ _ fmls res) -> do
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
                        fs_ty <- case ty' of
                                T.NAME name -> case S.look tenv name of
                                        Just ty'' -> isRecord ty''
                                        Nothing -> Err.returnErr_ (Err.TypeNotFound name) pos
                                T.RECORD _ fields -> return fields
                                _ -> Err.returnErr_ (Err.WrongType "record type" (show ty')) pos
                        checkrecord (map (\(x, y, z) -> (x, (y, z))) fields) fs_ty
                        return $ ExpTy () ty'
                    where
                        isRecord :: T.Ty -> Either Err.Error [(S.Symbol, T.Ty)]
                        isRecord ty' = case ty' of
                                T.NAME name -> case S.look tenv name of
                                        Just ty'' -> isRecord ty''
                                        Nothing -> Err.returnErr_ (Err.TypeNotFound name) pos
                                T.RECORD _ fields -> return fields
                                _ -> Err.returnErr_ (Err.WrongType "record type" (show ty')) pos
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
                        T.NAME name -> case S.look tenv name of
                                Just ty' -> isArray ty'
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
        regisTypeDec (A.TypeDec name ty pos) = StateT $ \st@(SS venv tenv _ _) -> do
                when (isJust $ S.look tenv name) (Err.returnErr_ (Err.MultipleDeclarations name) pos) --todo: local hides global
                let tenv' = S.enter tenv name (T.Temp ty)
                return ([(name, pos)], st{tenv = tenv'})
        regisTypeDec _ = return []
        regisFunDec :: A.Dec -> StateT SemantState (Either Err.Error) [FunDec]
        regisFunDec (A.FunDec name params result body pos) = StateT $ \st@(SS venv tenv lev tst) -> do
                when (isJust $ S.look venv name) (Err.returnErr_ (Err.MultipleDeclarations name) pos) --todo: local hides global
                params' <- forM params $ \(A.Field name esc typ pos) -> case S.look tenv typ of
                        Just ty -> return (name, ty, esc)
                        Nothing -> Err.returnErr_ (Err.TypeNotFound typ) pos
                result_ty <- case result of
                        Just (typ, pos) -> case S.look tenv typ of
                                Nothing -> Err.returnErr_ (Err.TypeNotFound typ) pos
                                Just result_ty -> return result_ty
                        Nothing -> return T.UNIT
                let (lev', tst') = runState (TL.newLevel lev (map A.fieldEscape params)) tst
                    fun = E.FunEntry lev' (TL.name lev') (map (\(_, x, _) -> x) params') result_ty
                    venv' = S.enter venv name fun
                return ([(name, params', result_ty, body, pos)], st{venv = venv'})
        regisFunDec _ = return []
        transTypeDec :: TypeDec -> StateT SemantState (Either Err.Error) ()
        transTypeDec (name, pos) = StateT $ \st -> do
                st' <- execStateT (transTy (name, pos)) st
                return ((), st')
        transFunDec :: FunDec -> StateT SemantState (Either Err.Error) ()
        transFunDec (name, params', result_ty, body, pos) = StateT $ \st@(SS venv tenv lev tst) -> do
                let enterparam :: (S.Symbol, T.Ty, Bool) -> State SemantState ()
                    enterparam (name, ty, esc) = do
                        st@(SS{venv = venv}) <- get
                        let (acs, tst') = runState (TL.allocLocal lev esc) tst
                            venv' = S.enter venv name (E.VarEntry acs ty)
                        put st{venv = venv'}
                        return ()
                    fun = case S.look venv name of
                        Just fun -> fun
                        _ -> undefined
                    st' = execState (mapM enterparam params') st{level = E.level fun}
                ty <- ty <$> transExp st' body
                match result_ty ty tenv pos
                return ((), st)
        transVarDec :: A.Dec -> StateT SemantState (Either Err.Error) ()
        transVarDec (A.VarDec name esc mtyp init pos) = StateT $ \st@(SS venv tenv lev tst) -> case mtyp of
                Just (typ, p) -> case S.look tenv typ of
                        Nothing -> Err.returnErr_ (Err.TypeNotFound typ) pos
                        Just t -> do
                                ty <- ty <$> transExp st init
                                match t ty tenv pos
                                let (acs, tst') = runState (TL.allocLocal lev esc) tst
                                    venv' = S.enter venv name (E.VarEntry acs ty)
                                return ((), st{venv = venv', tstate = tst'})
                Nothing -> do
                        ty <- ty <$> transExp st init
                        let (acs, tst') = runState (TL.allocLocal lev esc) tst
                            venv' = S.enter venv name (E.VarEntry acs ty)
                        return ((), st{venv = venv', tstate = tst'})
        transVarDec _ = return ()

transTy :: (A.Symbol, A.Pos) -> StateT SemantState (Either Err.Error) T.Ty
transTy (name, pos) = StateT $ \st@(SS _ tenv _ _) -> do
        (ty, tenv') <- transTy' [] (name, pos) tenv
        return (ty, st{tenv = S.enter tenv' name ty})

transTy' :: [A.Symbol] -> (A.Symbol, A.Pos) -> TEnv -> Either Err.Error (T.Ty, TEnv)
transTy' checked (name, pos) tenv = case S.look tenv name of
        Just (T.Temp (A.NameTy typ p)) -> do
                when (name `elem` checked) (Err.returnErr_ (Err.CyclicDefinition name) pos)
                (ty, tenv') <- transTy' (checked ++ [name]) (typ, pos) tenv
                return (ty, tenv')
        Just (T.Temp (A.RecordTy fields))
                | name `elem` checked -> return (T.Ref name, tenv)
                | otherwise -> do
                        fs_ty <- forM fields $ \(A.Field nam _ typ p) -> do
                                (ty, _) <- transTy' (checked ++ [name]) (typ, p) tenv
                                return (nam, ty)
                        return (T.RECORD name fs_ty, S.enter tenv name (T.RECORD name fs_ty))
        Just (T.Temp (A.ArrayTy typ p))
                | name `elem` checked -> return (T.Ref name, tenv)
                | otherwise -> do
                        (ty, tenv') <- transTy' (checked ++ [name]) (typ, p) tenv
                        return (T.ARRAY name ty, S.enter tenv' typ ty)
        Just ty -> return (ty, tenv)
        _ -> Err.returnErr_ (Err.TypeNotFound name) pos

transProg :: A.Exp -> Either Err.Error ()
transProg exp = do
        transExp (SS E.baseVEnv E.baseTEnv TL.Outermost Temp.initState) exp
        return ()
