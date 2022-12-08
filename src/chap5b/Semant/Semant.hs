{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Semant.Semant where

import Control.Monad.Reader
import Data.IORef
import Data.List (find)
import Prettyprinter
import Prettyprinter.Render.String

import Semant.Env
import Semant.Symbol
import Semant.Types as T
import Syntax.Absyn as A

-- | Environment
type VEnv = Table EnvEntry

type TEnv = Table T.Ty

data ExpTy = ExpTy {expty_exp :: (), expty_ty :: T.Ty}

-- | Monad
data Env = Env {s_venv :: VEnv, s_tenv :: TEnv, s_unique :: IORef Unique}

type Semant m = ReaderT Env m

failSemant :: MonadFail m => Pos -> Doc ann -> m a
failSemant (Pos l c) doc =
        fail $
                show l ++ ":" ++ show c ++ ": "
                        ++ renderString (layoutPretty defaultLayoutOptions doc)

-- | Utilities
check :: (MonadFail m, MonadIO m) => T.Ty -> T.Ty -> Pos -> Semant m ()
check lty rty pos = do
        tenv <- asks s_tenv
        lty' <- actualTy pos tenv lty
        rty' <- actualTy pos tenv rty
        if lty' == rty' || NIL == lty' || NIL == rty'
                then return ()
                else failSemant pos $ hsep ["type mismatch: expected", squotes (pretty lty) <> comma, "but got", squotes (pretty rty)]

actualTy :: (MonadFail m, MonadIO m) => Pos -> TEnv -> T.Ty -> m T.Ty
actualTy pos tenv = walk1
    where
        walk1 :: (MonadFail m, MonadIO m) => T.Ty -> m T.Ty
        walk1 ty = case ty of
                NAME typ _ -> case look typ tenv of
                        Just ty' -> walk1 ty'
                        Nothing -> failSemant pos $ hsep ["type not found:", pretty typ]
                ARRAY ty' u -> ARRAY <$> walk2 ty' <*> pure u
                RECORD fields u -> RECORD <$> mapM (\(lab, ty) -> (lab,) <$> walk2 ty) fields <*> pure u
                _ -> return ty
        walk2 :: MonadFail m => T.Ty -> m T.Ty
        walk2 ty = case ty of
                NAME{} -> return ty
                ARRAY ty' u -> ARRAY <$> walk2 ty' <*> pure u
                RECORD fields u -> RECORD <$> mapM (\(lab, ty) -> (lab,) <$> walk2 ty) fields <*> pure u
                _ -> return ty

uniqueNameCheck :: MonadFail m => [(Name, Pos)] -> m ()
uniqueNameCheck [] = return ()
uniqueNameCheck ((x1, _) : xs) = case find ((x1 ==) . fst) xs of
        Just (x2, p2) -> failSemant p2 $ hsep ["multiple declaration for", squotes (pretty x2)]
        Nothing -> uniqueNameCheck xs

newUnique :: MonadIO m => Semant m Unique
newUnique = do
        ref <- asks s_unique
        uniq <- liftIO $ readIORef ref
        liftIO $ writeIORef ref (uniq + 1)
        return uniq

-- | Translating Var
transVar :: (MonadFail m, MonadIO m) => Var -> Semant m ExpTy
transVar (SimpleVar pos x) = do
        venv <- asks s_venv
        case look x venv of
                Nothing -> failSemant pos $ hsep ["undefined variable:", pretty x]
                Just (VarEntry ty) -> return $ ExpTy () ty
                Just FunEntry{} -> failSemant pos $ hsep ["undefined variable:", pretty x]
transVar (FieldVar pos var x) = do
        ty <- expty_ty <$> transVar var
        case ty of
                RECORD fields _ -> case lookup x fields of
                        Nothing -> failSemant pos $ hsep ["label not found:", pretty x]
                        Just ty' -> return $ ExpTy () ty'
                _ -> failSemant pos $ hsep ["record type required:", pretty var]
transVar (SubscriptVar pos var exp) = do
        ty <- expty_ty <$> transVar var
        case ty of
                ARRAY ty' _ -> do
                        ty'' <- expty_ty <$> transExp exp
                        check INT ty'' pos
                        return $ ExpTy () ty'
                _ -> failSemant pos $ hsep ["array type required:", pretty var]

-- | Translating Exp
transExp :: (MonadFail m, MonadIO m) => Exp -> Semant m ExpTy
transExp (VarExp var) = transVar var
transExp NilExp = return $ ExpTy () NIL
transExp (IntExp _) = return $ ExpTy () INT
transExp (StringExp _ _) = return $ ExpTy () STRING
transExp (CallExp pos fun args) = do
        venv <- asks s_venv
        case look fun venv of
                Nothing -> failSemant pos $ hsep ["undefined variable:", pretty fun]
                Just VarEntry{} -> failSemant pos $ hsep ["undefined variable:", pretty fun]
                Just (FunEntry fmls res) -> do
                        checkformals args fmls
                        return $ ExpTy () res
    where
        checkformals :: (MonadFail m, MonadIO m) => [Exp] -> [T.Ty] -> Semant m ()
        checkformals [] [] = return ()
        checkformals (e : es) (t : ts) = do
                ty <- expty_ty <$> transExp e
                check t ty pos
                checkformals es ts
        checkformals _ _ = failSemant pos $ hsep ["wrong number of arguments:", pretty fun]
transExp (OpExp pos lhs op rhs) = do
        lty <- expty_ty <$> transExp lhs
        rty <- expty_ty <$> transExp rhs
        if case (lty, rty) of
                (INT, INT) -> True
                (STRING, STRING)
                        | op `notElem` [PlusOp, MinusOp, TimesOp, DivideOp] -> True
                        | otherwise -> False
                (ty, ty')
                        | op `elem` [EqOp, NeqOp] && ty == ty' -> True
                        | otherwise -> False
                then return $ ExpTy () INT
                else failSemant pos $ hsep ["invalid comparison of", squotes (pretty op) <> colon, pretty lty, "with", pretty rty]
transExp (RecordExp pos fields typ) = do
        tenv <- asks s_tenv
        case look typ tenv of
                Nothing -> failSemant pos $ hsep ["type not found:", pretty typ]
                Just rec_ty -> do
                        rec_ty' <- actualTy pos tenv rec_ty
                        fields_ty <- case rec_ty' of
                                RECORD fields _ -> return fields
                                _ -> failSemant pos $ hsep ["record type required:", pretty typ]
                        checkRecord fields_ty
                        return $ ExpTy () rec_ty'
                    where
                        fields' = map (\(x, y, z) -> (x, (y, z))) fields
                        checkRecord :: (MonadFail m, MonadIO m) => [(Symbol, T.Ty)] -> Semant m ()
                        checkRecord [] = return ()
                        checkRecord ((l, t) : ftys) = case lookup l fields' of
                                Just (e, p) -> do
                                        ty <- expty_ty <$> transExp e
                                        check t ty p
                                        checkRecord ftys
                                Nothing -> failSemant pos $ hsep ["label not found:", pretty l]
transExp (SeqExp seqexp) = case reverse seqexp of
        [] -> return $ ExpTy () UNIT
        e : es -> do
                mapM_ transExp (reverse es)
                transExp e
transExp (AssignExp pos var exp) = do
        var_ty <- expty_ty <$> transVar var
        exp_ty <- expty_ty <$> transExp exp
        check var_ty exp_ty pos
        return $ ExpTy () UNIT
transExp (IfExp pos test then' else') = do
        test_ty <- expty_ty <$> transExp test
        check INT test_ty pos
        then_ty <- expty_ty <$> transExp then'
        case else' of
                Nothing -> do
                        check UNIT then_ty pos
                        return $ ExpTy () UNIT
                Just else'' -> do
                        else_ty <- expty_ty <$> transExp else''
                        check then_ty else_ty pos
                        return $ ExpTy () then_ty
transExp (WhileExp pos test body) = do
        test_ty <- expty_ty <$> transExp test
        check INT test_ty pos
        body_ty <- expty_ty <$> transExp body
        check UNIT body_ty pos
        return $ ExpTy () UNIT
transExp (ForExp pos _ _ lo hi body) = do
        lo_ty <- expty_ty <$> transExp lo
        check INT lo_ty pos
        hi_ty <- expty_ty <$> transExp hi
        check INT hi_ty pos
        transExp body
transExp (BreakExp _) = return $ ExpTy () UNIT
transExp (LetExp _ decs body) = do
        env <- transDecs decs
        local (const env) $ transExp body
transExp (ArrayExp pos typ size init) = do
        tenv <- asks s_tenv
        case look typ tenv of
                Nothing -> failSemant pos $ hsep ["type not found:", pretty typ]
                Just arr_ty -> do
                        arr_ty' <- actualTy pos tenv arr_ty
                        ty <- case arr_ty' of
                                ARRAY ty _ -> return ty
                                _ -> failSemant pos $ hsep ["array type required:", pretty typ]
                        size_ty <- expty_ty <$> transExp size
                        check INT size_ty pos
                        init_ty <- expty_ty <$> transExp init
                        check ty init_ty pos
                        return $ ExpTy () arr_ty

-- | Translating Dec
transDecs :: (MonadFail m, MonadIO m) => [Dec] -> Semant m Env
transDecs [] = ask
transDecs (dec : decs) = do
        env <- ask
        env' <- case dec of
                TypeDec tydecs -> do
                        uniqueNameCheck [(typ, pos) | (typ, _, pos) <- tydecs]
                        tenv <- asks s_tenv
                        ref <- liftIO $ newIORef Nothing
                        let tenv' = foldl (\tenv typ -> enter typ (NAME typ ref) tenv) tenv [typ | (typ, _, _) <- tydecs]
                        local (const env{s_tenv = tenv'}) $ transTypeDec tydecs
                FunctionDec fundecs -> do
                        uniqueNameCheck [(name, pos) | FunDec pos name _ _ _ <- fundecs]
                        venv <- enterHeaders fundecs
                        local (const env{s_venv = venv}) $ transFunDec fundecs
                        return env{s_venv = venv}
                VarDec _ name _ Nothing init -> do
                        init_ty <- expty_ty <$> transExp init
                        return env{s_venv = enter name (VarEntry init_ty) (s_venv env)}
                VarDec pos name _ (Just (typ, pos')) init -> do
                        init_ty <- expty_ty <$> transExp init
                        tenv <- asks s_tenv
                        res_ty <- case look typ tenv of
                                Nothing -> failSemant pos' $ hsep ["type not found:", pretty typ]
                                Just res_ty -> return res_ty
                        check res_ty init_ty pos
                        return env{s_venv = enter name (VarEntry res_ty) (s_venv env)}
        local (const env') $ transDecs decs
    where
        transTypeDec :: (MonadFail m, MonadIO m) => [(Name, A.Ty, Pos)] -> Semant m Env
        transTypeDec [] = ask
        transTypeDec ((typ, ty_abs, pos) : tydecs) = do
                tenv <- asks s_tenv
                ty_sem <- transTy ty_abs
                case look typ tenv of
                        Just (NAME _ ref) -> liftIO $ writeIORef ref (Just ty_sem)
                        _ -> failSemant pos "bug: unreachable"
                let tenv' = enter typ ty_sem tenv
                local (\env -> env{s_tenv = tenv'}) $ transTypeDec tydecs
        enterHeaders :: (MonadFail m, MonadIO m) => [FunDec] -> Semant m VEnv
        enterHeaders [] = asks s_venv
        enterHeaders ((FunDec _ name params res _) : fundecs) = do
                env@(Env venv tenv _) <- ask
                uniqueNameCheck [(lab, pos) | Field pos lab _ _ <- params]
                params' <- forM params $ \(Field pos lab esc typ) -> case look typ tenv of
                        Just ty -> return (lab, ty, esc)
                        Nothing -> failSemant pos $ hsep ["type not found:", pretty typ]
                res_ty <- case res of
                        Just (typ, pos) -> case look typ tenv of
                                Nothing -> failSemant pos $ hsep ["type not found:", pretty typ]
                                Just res_ty -> return res_ty
                        Nothing -> return UNIT
                let venv' = enter name (FunEntry (map (\(_, ty, _) -> ty) params') res_ty) venv
                local (const env{s_venv = venv'}) $ enterHeaders fundecs
        transFunDec :: (MonadFail m, MonadIO m) => [FunDec] -> Semant m ()
        transFunDec [] = return ()
        transFunDec ((FunDec pos _ params res body) : fundecs) = do
                env@(Env venv tenv _) <- ask
                params' <- forM params $ \(Field pos lab esc typ) -> case look typ tenv of
                        Just ty -> return (lab, ty, esc)
                        Nothing -> failSemant pos $ hsep ["type not found:", pretty typ]
                res_ty <- case res of
                        Just (typ, pos) -> case look typ tenv of
                                Nothing -> failSemant pos $ hsep ["type not found:", pretty typ]
                                Just res_ty -> return res_ty
                        Nothing -> return UNIT
                let venv' = foldl (\venv (n, ty, _) -> enter n (VarEntry ty) venv) venv params'
                body_ty <- local (const env{s_venv = venv'}) $ expty_ty <$> transExp body
                check res_ty body_ty pos
                transFunDec fundecs

-- | Translating Ty
transTy :: (MonadFail m, MonadIO m) => A.Ty -> Semant m T.Ty
transTy ty = do
        tenv <- asks s_tenv
        case ty of
                NameTy pos typ -> case look typ tenv of
                        Just ty -> return ty
                        Nothing -> failSemant pos $ hsep ["type not found:", pretty typ]
                RecordTy fields -> do
                        fields_ty <- forM fields $ \(Field pos lab _ typ) -> case look typ tenv of
                                Just ty -> return (lab, ty)
                                Nothing -> failSemant pos $ hsep ["type not found:", pretty typ]
                        RECORD fields_ty <$> newUnique
                ArrayTy pos typ -> case look typ tenv of
                        Just ty -> ARRAY ty <$> newUnique
                        Nothing -> failSemant pos $ hsep ["type not found:", pretty typ]

transProg :: (MonadFail m, MonadIO m) => Exp -> m ()
transProg exp = do
        ref <- liftIO $ newIORef 0
        void $ runReaderT (transExp exp) (Env baseVEnv baseTEnv ref)