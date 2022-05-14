module Semant.Semant where

import qualified Common.Symbol as S
import qualified Common.Temp as Temp
import qualified Semant.Env as E
import qualified Semant.Error as Err
import qualified Semant.Translate as TL
import qualified Semant.Types as T
import qualified Syntax.Absyn as A
import qualified Syntax.Absyn.Utils as A

import Control.Monad.State
import Data.List (elemIndex)

type VEnv = S.Table E.EnvEntry
type TEnv = S.Table (TL.Level, T.Ty)

data ExpTy = ExpTy {exptyExp :: TL.Exp, exptyTy :: T.Ty} deriving (Show)

------------------------------------------------------------------
-- SemantState
------------------------------------------------------------------
data SemantState = SS {venv :: VEnv, tenv :: TEnv, level :: TL.Level, tstate :: Temp.TempState}

convTempState :: State Temp.TempState a -> StateT SemantState (Either Err.Error) a
convTempState s = do
        st <- get
        tst <- gets tstate
        let (res, tst') = runState s tst
        put st{tstate = tst'}
        return res

convVEnv :: Monad m => StateT VEnv m a -> StateT SemantState m a
convVEnv s = do
        st <- get
        venv <- gets venv
        (val, venv') <- lift $ runStateT s venv
        put st{venv = venv'}
        return val

convTEnv :: Monad m => StateT TEnv m a -> StateT SemantState m a
convTEnv s = do
        st <- get
        tenv <- gets tenv
        (val, tenv') <- lift $ runStateT s tenv
        put st{tenv = tenv'}
        return val

initState :: SemantState
initState = SS E.baseVEnv E.baseTEnv TL.topLevel Temp.emptyState

------------------------------------------------------------------
-- Utils
------------------------------------------------------------------
match :: T.Ty -> T.Ty -> TEnv -> A.Pos -> Either Err.Error ()
match lty rty tenv pos = do
        lty' <- actualTy lty
        rty' <- actualTy rty
        if lty' == rty' || T.NIL == lty' || T.NIL == rty'
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
transVar :: SemantState -> A.Var -> StateT Temp.TempState (Either Err.Error) ExpTy
transVar st@(SS venv tenv lev tst) = trvar
    where
        trvar :: A.Var -> StateT Temp.TempState (Either Err.Error) ExpTy
        trvar (A.SimpleVar sym pos) = case S.look venv sym of
                Nothing -> lift $ Err.unknownVariable sym pos
                Just (E.VarEntry acs ty) -> return $ ExpTy (TL.simpleVar (acs, lev)) ty
                Just E.FunEntry{} -> lift $ Err.unknownVariable sym pos
        trvar (A.FieldVar var sym pos) = do
                ExpTy var' ty <- trvar var
                case ty of
                        T.RECORD _ fs -> case lookup sym fs of
                                Nothing -> lift $ Err.otherError ("field not found" ++ sym) pos
                                Just ty' -> do
                                        let Just idx = elemIndex sym (map fst fs)
                                        expr <- mkT $ TL.lvalueVar var' (TL.intExp idx)
                                        return $ ExpTy expr ty'
                        _ -> lift $ Err.wrongType "record type" (show ty) pos
        trvar (A.SubscriptVar var exp pos) = do
                ExpTy var' ty <- trvar var
                case ty of
                        T.ARRAY _ ty' -> do
                                ExpTy idx ty'' <- lift $transExp st exp
                                lift $ match T.INT ty'' tenv pos
                                expr <- mkT $ TL.lvalueVar var' idx
                                return $ ExpTy expr ty'
                        _ -> lift $ Err.wrongType "array type" (show var) pos

------------------------------------------------------------------
-- Translate expressions
------------------------------------------------------------------
mkT :: State Temp.TempState a -> StateT Temp.TempState (Either Err.Error) a
mkT s = StateT $ \a -> return (runState s a)

transExp :: SemantState -> A.Exp -> Either Err.Error ExpTy
transExp st@(SS venv tenv lev tst) exp = evalStateT (trexp exp) tst
    where
        trexp :: A.Exp -> StateT Temp.TempState (Either Err.Error) ExpTy --tmp: StateT, evalState? tempstate
        trexp (A.VarExp v) = transVar st v
        trexp A.NilExp = return $ ExpTy TL.nilExp T.NIL
        trexp (A.IntExp i) = return $ ExpTy (TL.intExp i) T.INT
        trexp (A.StringExp (s, p)) = do
                (expr, _) <- mkT $ TL.stringExp s
                return $ ExpTy expr T.STRING
        trexp (A.CallExp fun args pos) = case S.look venv fun of
                Nothing -> lift $ Err.unknownFunction fun pos
                Just (E.VarEntry _ _) -> lift $ Err.unknownFunction fun pos
                Just (E.FunEntry fun_lev lab fmls res) -> do
                        lift $when (length args /= length fmls) $ Err.wrongNumberArgs (length args) (length fmls) pos
                        zipWithM_ checkformal args fmls
                        args' <- mapM trexp args
                        expr <- mkT $ TL.callExp (TL.parent fun_lev, lev) lab (map exptyExp args')
                        return $ ExpTy expr res
                    where
                        checkformal :: A.Exp -> T.Ty -> StateT Temp.TempState (Either Err.Error) ()
                        checkformal e t = do
                                ty <- exptyTy <$> trexp e
                                lift $ match t ty tenv pos
        trexp (A.OpExp left op right pos) = do
                ExpTy left' lty <- trexp left -- tmp: string comparison
                ExpTy right' rty <- trexp right
                kind <- case (lty, rty) of
                        (T.INT, T.INT) -> return 0
                        (T.STRING, T.STRING) -> return 1
                        (t, t') -> case A.opkind op of
                                A.Equal | t == t' -> return 2
                                _ -> lift $ Err.invalidComparison (show lty) (show rty) (show op) pos
                expr <- case op of
                        A.PlusOp | kind < 1 -> mkT $ left' `TL.plusOp` right'
                        A.MinusOp | kind < 1 -> mkT $ left' `TL.minusOp` right'
                        A.TimesOp | kind < 1 -> mkT $ left' `TL.timesOp` right'
                        A.DivideOp | kind < 1 -> mkT $ left' `TL.divideOp` right'
                        A.LtOp | kind < 2 -> mkT $ left' `TL.ltOp` right'
                        A.LeOp | kind < 2 -> mkT $ left' `TL.leOp` right'
                        A.GtOp | kind < 2 -> mkT $ left' `TL.gtOp` right'
                        A.GeOp | kind < 2 -> mkT $ left' `TL.geOp` right'
                        A.EqOp | kind < 3 -> mkT $ left' `TL.eqOp` right'
                        A.NeqOp | kind < 3 -> mkT $ left' `TL.neqOp` right'
                        _ -> lift $ Err.invalidComparison (show lty) (show rty) (show op) pos
                return $ ExpTy expr T.INT
        trexp (A.RecordExp fields typ pos) = do
                ty <- lift $lookty tenv typ pos
                fs_ty <- case ty of
                        T.NAME name -> do
                                ty' <- lift $ lookty tenv name pos
                                lift $isRecord ty'
                        T.RECORD _ fields -> return fields
                        _ -> lift $ Err.wrongType "record type" (show ty) pos
                checkrecord (map (\(x, y, z) -> (x, (y, z))) fields) fs_ty
                exps <- map exptyExp <$> mapM (trexp . (\(x, y, z) -> y)) fields
                expr <- mkT $ TL.recordExp exps
                return $ ExpTy expr ty
            where
                isRecord :: T.Ty -> Either Err.Error [(S.Symbol, T.Ty)]
                isRecord ty = case ty of
                        T.NAME name -> do
                                ty' <- lookty tenv name pos
                                isRecord ty'
                        T.RECORD _ fields -> return fields
                        _ -> Err.wrongType "record type" (show ty) pos
                checkrecord :: [(A.Symbol, (A.Exp, A.Pos))] -> [(S.Symbol, T.Ty)] -> StateT Temp.TempState (Either Err.Error) ()
                checkrecord _ [] = return ()
                checkrecord fs ((s, t) : sts) = case lookup s fs of
                        Just (e, p) -> do
                                ty <- exptyTy <$> trexp e
                                lift $ match t ty tenv pos
                                checkrecord fs sts
                        Nothing -> lift $ Err.otherError ("record field not found" ++ s) pos
        trexp (A.SeqExp seqexp pos) = do
                exps <- map exptyExp <$> mapM trexp seqexp
                expr <- mkT $ TL.seqExp exps
                ty <- if null seqexp then return T.UNIT else exptyTy <$> trexp (last seqexp)
                return $ ExpTy expr ty
        trexp (A.AssignExp v e pos) = do
                ExpTy left lty <- transVar st v
                ExpTy right rty <- trexp e
                lift $ match lty rty tenv pos
                expr <- mkT $ TL.assignExp left right
                return $ ExpTy expr T.UNIT
        trexp (A.IfExp test then' melse pos) = do
                ExpTy test' test_ty <- trexp test
                lift $ match T.INT test_ty tenv pos
                ExpTy then'' then_ty <- trexp then'
                case melse of
                        Nothing -> do
                                lift $ match T.UNIT then_ty tenv pos
                                expr <- mkT $ TL.ifExp test' then'' TL.nilExp
                                return $ ExpTy expr T.UNIT
                        Just else' -> do
                                ExpTy else'' else_ty <- trexp else'
                                lift $ match then_ty else_ty tenv pos
                                expr <- mkT $ TL.ifExp test' then'' else''
                                return $ ExpTy expr then_ty
        trexp (A.WhileExp test body pos) = do
                ExpTy test' test_ty <- trexp test
                lift $ (T.INT `match` test_ty) tenv pos
                ExpTy body' body_ty <- trexp body
                lift $ (T.UNIT `match` body_ty) tenv pos
                expr <- mkT $ TL.whileExp test' body'
                return $ ExpTy expr T.UNIT
        trexp (A.ForExp name esc lo hi body pos) = do
                lo_ty <- exptyTy <$> trexp lo
                lift $ (T.INT `match` lo_ty) tenv pos
                hi_ty <- exptyTy <$> trexp hi
                lift $ (T.INT `match` hi_ty) tenv pos
                let e = A.forToLet (name, esc, lo, hi, body, pos)
                trexp e
        trexp (A.BreakExp _) = error "break is not implemented yet"
        trexp (A.LetExp decs body pos) = do
                (decs', st') <- lift $transDecs decs `runStateT` st
                ExpTy body' ty <- lift $transExp st' body
                expr <- mkT $ TL.letExp decs' body'
                return $ ExpTy expr ty
        trexp (A.ArrayExp typ size init pos) = do
                ty <- lift $lookty tenv typ pos
                ty' <- lift $isArray ty
                ExpTy size' size_ty <- trexp size
                lift $ (T.INT `match` size_ty) tenv pos
                ExpTy init' init_ty <- trexp init
                lift $ (ty' `match` init_ty) tenv pos
                expr <- mkT $ TL.arrayExp size' init'
                return $ ExpTy expr ty
            where
                isArray :: T.Ty -> Either Err.Error T.Ty
                isArray ty = case ty of
                        T.NAME name -> do
                                ty' <- lookty tenv name pos
                                isArray ty'
                        T.ARRAY _ ty' -> return ty'
                        _ -> Err.wrongType "array type" (show ty) pos

------------------------------------------------------------------
-- Translate declarations
------------------------------------------------------------------
type TypeDec = (A.Symbol, A.Pos)
type FunDec = (S.Symbol, [(S.Symbol, T.Ty, Bool)], T.Ty, A.Exp, A.Pos)

transDecs :: [A.Dec] -> StateT SemantState (Either Err.Error) [TL.Exp]
transDecs decs = do
        typedecs <- concat <$> mapM regisTypeDec decs
        fundecs <- concat <$> mapM regisFunDec decs
        mapM_ transTypeDec typedecs
        mapM_ transFunDec fundecs
        mapM_ transVarDec decs
        concat <$> mapM transVarDec decs
    where
        regisTypeDec :: A.Dec -> StateT SemantState (Either Err.Error) [TypeDec]
        regisTypeDec (A.TypeDec name ty pos) = do
                tenv <- gets tenv
                lev <- gets level
                lift $ case S.look tenv name of
                        Just (lev_dec, ty) | TL.name lev == TL.name lev_dec -> Err.returnErr_ (Err.MultipleDeclarations name) pos
                        _ -> return ()
                convTEnv $ S.enter name (lev, T.Temp ty)
                return [(name, pos)]
        regisTypeDec _ = return []
        regisFunDec :: A.Dec -> StateT SemantState (Either Err.Error) [FunDec]
        regisFunDec (A.FunDec name params result body pos) = do
                venv <- gets venv
                lev <- gets level
                lift $ case S.look venv name of
                        Just E.FunEntry{E.level = lev_dec} | TL.name lev_dec == TL.name lev -> Err.returnErr_ (Err.MultipleDeclarations name) pos
                        _ -> return ()
                tenv <- gets tenv
                params' <- lift $
                        forM params $ \(A.Field name esc typ pos) -> do
                                ty <- lookty tenv typ pos
                                return (name, ty, esc)
                result_ty <- lift $ case result of
                        Just (typ, pos) -> lookty tenv typ pos
                        Nothing -> return T.UNIT
                lev' <- convTempState (TL.newLevel lev (map A.fieldEscape params))
                let fun = E.FunEntry lev' (Temp.namedLabel name) (map (\(_, x, _) -> x) params') result_ty
                convVEnv $ S.enter name fun
                return [(name, params', result_ty, body, pos)]
        regisFunDec _ = return []
        transTypeDec :: TypeDec -> StateT SemantState (Either Err.Error) ()
        transTypeDec (name, pos) = void $ transTy (name, pos)
        transFunDec :: FunDec -> StateT SemantState (Either Err.Error) ()
        transFunDec (name, params', result_ty, body, pos) = do
                st@(SS venv tenv _ _) <- get
                let enterparam :: (S.Symbol, T.Ty, Bool) -> State SemantState ()
                    enterparam (name, ty, esc) = do
                        st@(SS venv _ lev tst) <- get
                        let (acs, tst') = runState (TL.allocLocal lev esc) tst
                        convVEnv $ S.enter name (E.VarEntry acs ty)
                        return ()
                    Just fun = S.look venv name
                    st' = mapM enterparam params' `execState` st{level = E.level fun}
                ty <- lift $ exptyTy <$> transExp st' body
                lift $ match result_ty ty tenv pos
                return ()
        transVarDec :: A.Dec -> StateT SemantState (Either Err.Error) [TL.Exp]
        transVarDec (A.VarDec name esc mtyp init pos) = do
                st <- get
                tenv <- gets tenv
                (expr, ty) <- lift $ case mtyp of
                        Just (typ, p) -> do
                                ty <- lookty tenv typ pos
                                ExpTy expr ty' <- transExp st init
                                match ty ty' tenv pos
                                return (expr, ty)
                        Nothing -> do
                                ExpTy expr ty <- transExp st init
                                return (expr, ty)
                lev <- gets level
                acs <- convTempState $ TL.allocLocal lev esc
                convVEnv $ S.enter name (E.VarEntry acs ty)
                let var = TL.simpleVar (acs, lev)
                decl <- convTempState $ TL.assignExp var expr
                return [decl]
        transVarDec _ = return []

------------------------------------------------------------------
-- Translate types
------------------------------------------------------------------
transTy :: (A.Symbol, A.Pos) -> StateT SemantState (Either Err.Error) T.Ty
transTy (name, pos) = do
        tenv <- gets tenv
        ty <- convTEnv $ transTy' [] (name, pos)
        lev <- gets level
        convTEnv $ S.enter name (lev, ty)
        return ty

transTy' :: [A.Symbol] -> (A.Symbol, A.Pos) -> StateT TEnv (Either Err.Error) T.Ty
transTy' checked (name, pos) = do
        tenv <- get
        case S.look tenv name of
                Just (_, T.Temp (A.NameTy typ p)) -> do
                        lift $ when (name `elem` checked) $ Err.returnErr_ (Err.CyclicDefinition name) pos
                        transTy' (checked ++ [name]) (typ, pos)
                Just (lev, T.Temp (A.RecordTy fields))
                        | name `elem` checked -> return $ T.Ref name
                        | otherwise -> do
                                fs_ty <- forM fields $ \(A.Field nam _ typ p) -> do
                                        ty <- transTy' (checked ++ [name]) (typ, p)
                                        return (nam, ty)
                                S.enter name (lev, T.RECORD name fs_ty)
                                return (T.RECORD name fs_ty)
                Just (lev, T.Temp (A.ArrayTy typ p))
                        | name `elem` checked -> return $ T.Ref name
                        | otherwise -> do
                                ty <- transTy' (checked ++ [name]) (typ, p)
                                S.enter typ (lev, ty)
                                return (T.ARRAY name ty)
                Just (_, ty) -> return ty
                _ -> lift $ Err.unknownType name pos

transProg :: A.Exp -> Either Err.Error ()
transProg exp = do
        transExp initState exp
        return ()
