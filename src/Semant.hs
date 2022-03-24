module Semant where

import qualified Absyn as A
import qualified Env as E
import qualified Symbol as S
import qualified Translate
import qualified Types as T

type VEnv = S.Table E.EnvEntry

type TEnv = S.Table T.Ty

data ExpTy = ExpTy {exp :: Translate.Exp, ty :: T.Ty} deriving (Show)

actualTy :: T.Ty -> Maybe T.Ty
actualTy ty = case ty of
        T.NAME sym mty -> actualTy =<< mty
        T.ARRAY ty' u -> T.ARRAY <$> actualTy ty' <*> Just u
        _ -> Just ty

(@@) :: T.Ty -> T.Ty -> Bool
(@@) ty1 ty2 = actualTy ty1 == actualTy ty2 -- warning: Nothing == Nothing

checkUnit :: T.Ty -> Bool
checkUnit ty = ty @@ T.UNIT

transVar :: (VEnv, TEnv, A.Var) -> ExpTy
transVar (venv, tenv, var) = trvar var
    where
        trvar :: A.Var -> ExpTy
        trvar (A.SimpleVar sym pos) = case S.look venv sym of
                Nothing -> error $ show pos ++ "undefined variable: " ++ sym
                Just (E.VarEntry ty) -> ExpTy () ty
                Just (E.FunEntry _ _) -> error $ show pos ++ "not a variable: " ++ sym
        trvar (A.FieldVar v sym pos) = case ty $ trvar v of
                T.RECORD fs _ -> case lookup sym fs of
                        Nothing -> error $ show pos ++ "field not found: " ++ sym
                        Just ty -> ExpTy () ty
                ty -> error $ show pos ++ "not a record: " ++ show ty
        trvar (A.SubscriptVar v exp pos) = case ty $ trvar v of
                T.ARRAY ty' _
                        | ty (transExp (venv, tenv, exp)) @@ ty' -> ExpTy () ty'
                        | otherwise -> error $ show pos ++ "type not matched"
                ty' -> error $ show pos ++ "not an array: " ++ show ty'

transExp :: (VEnv, TEnv, A.Exp) -> ExpTy
transExp (venv, tenv, exp) = trexp exp
    where
        trexp :: A.Exp -> ExpTy
        trexp (A.VarExp v) = transVar (venv, tenv, v)
        trexp A.NilExp = ExpTy () T.NIL
        trexp (A.IntExp _) = ExpTy () T.INT
        trexp (A.StringExp _) = ExpTy () T.STRING
        trexp (A.CallExp func args pos) = case S.look venv func of
                Nothing -> error $ show pos ++ "undefined function: " ++ func
                Just (E.VarEntry ty) -> error $ show pos ++ "not a function (in actualTy): " ++ func
                Just (E.FunEntry formals result)
                        | length args /= length formals -> error $ show pos ++ "wrong number of arguments"
                        | checkFormals args formals -> case actualTy result of
                                Nothing -> error $ show pos ++ "type not found: " ++ show result
                                Just ty -> ExpTy () ty
                        | otherwise -> error $ show pos ++ "argument type not matched"
        trexp (A.OpExp left op right pos)
                | checkInt lty && checkInt rty = ExpTy () T.INT
                | isComp op && checkString lty && checkString rty = ExpTy () T.INT
                | isComp op = error $ show pos ++ "integer or string required"
                | otherwise = error $ show pos ++ "integer required"
            where
                lty = ty $ trexp left
                rty = ty $ trexp right
                isComp :: A.Oper -> Bool
                isComp op = op /= A.PlusOp && op /= A.MinusOp && op /= A.TimesOp && op /= A.DevideOp
        trexp (A.RecordExp fields typ pos) = case S.look tenv typ of
                Nothing -> error $ show pos ++ "record type not found: " ++ typ
                Just ty -> case actualTy ty of
                        Nothing -> error $ show pos ++ "type not found (in actualTy): " ++ show ty
                        Just (T.RECORD fs_ty u)
                                | length fields /= length fs_ty -> error $ show pos ++ "wrong number of field"
                                | and (checkRecord <$> fields <*> fs_ty) -> ExpTy () (T.RECORD fs_ty u)
                                | otherwise -> undefined
                        _ -> error $ show pos ++ "record type required"
        trexp (A.SeqExp seqexp) = case seqexp of
                [] -> undefined
                (e, p) : se -> trexp e --tmp
        trexp (A.AssignExp v exp pos) =
                if ty (transVar (venv, tenv, v)) @@ ty (trexp exp)
                        then ExpTy () T.UNIT
                        else error $ show pos ++ "var type not matched"
        trexp (A.IfExp test then' else' pos)
                | not $ checkInt $ ty $ trexp test = error $ show pos ++ "test must be integer"
                | otherwise = case else' of
                        Nothing
                                | checkUnit (ty $ trexp then') -> ExpTy () T.UNIT
                                | otherwise -> error $ show pos ++ "then must be unit when else is none"
                        Just else''
                                | ty (trexp then') @@ ty (trexp else'') -> trexp then'
                                | otherwise -> error $ show pos ++ "type not matched between then and else"
        trexp (A.WhileExp test body pos)
                | not $ checkInt $ ty $ trexp test = error $ show pos ++ "test must be integer"
                | checkUnit $ ty $ trexp body = ExpTy () T.UNIT
                | otherwise = error $ show pos ++ "body must be unit"
        trexp (A.ForExp name _ lo hi body pos)
                | not (checkInt $ ty $ trexp lo) || not (checkInt $ ty $ trexp hi) = error $ show pos ++ "integer required"
                | checkUnit $ ty $ trexp body = ExpTy () T.UNIT
                | otherwise = error $ show pos ++ "body must be unit"
        trexp (A.BreakExp _) = ExpTy () T.UNIT
        trexp (A.LetExp decs body pos) = transExp (venv', tenv', body)
            where
                (venv', tenv') = transDecs (venv, tenv, decs)
        trexp (A.ArrayExp typ size init pos) = case S.look tenv typ of
                Nothing -> error $ show pos ++ "array type not found: " ++ typ
                Just t -> case actualTy t of
                        Nothing -> error $ show pos ++ "type not found (in actualTy): " ++ show t
                        Just (T.ARRAY t' u)
                                | checkInt t' -> error $ show pos ++ "array size must be integer"
                                | ty (trexp init) @@ t' -> error $ show pos ++ "type not matched"
                                | otherwise -> ExpTy () t'
                        _ -> error $ show pos ++ "array type required"

        checkInt :: T.Ty -> Bool
        checkInt ty = ty @@ T.INT

        checkString :: T.Ty -> Bool
        checkString ty = ty @@ T.STRING

        checkRecord :: (A.Symbol, A.Exp, A.Pos) -> (S.Symbol, T.Ty) -> Bool
        checkRecord (s, e, p) (s', t) =
                (ty (trexp e) @@ t)
                        || error (show p ++ "record type not matched")

        checkFormals :: [A.Exp] -> [T.Ty] -> Bool
        checkFormals args formals = and $ (@@) <$> (ty . trexp <$> args) <*> formals

transDecs :: (VEnv, TEnv, [A.Dec]) -> (VEnv, TEnv)
transDecs (venv, tenv, []) = (venv, tenv)
transDecs (venv, tenv, dec : decs) = transDecs (venv', tenv', decs)
    where
        (venv', tenv') = transDec (venv, tenv, dec)

transDec :: (VEnv, TEnv, A.Dec) -> (VEnv, TEnv)
transDec (venv, tenv, A.FunctionDec fundecs) = case fundecs of
        [] -> error ""
        A.FunDec name params result body pos : fs -> case result of
                Just (typ, pos) -> case S.look tenv typ of
                        Nothing -> error $ show pos ++ "type not found: " ++ typ
                        Just result_ty
                                | result_ty @@ ty (transExp (venv'', tenv, body)) -> (venv', tenv)
                                | otherwise -> error $ show pos ++ "result type not matched"
                            where
                                venv' = S.enter venv name (E.FunEntry (map snd params') result_ty)
                                venv'' = foldr enterparam venv' params'
                Nothing
                        | checkUnit (ty $ transExp (venv'', tenv, body)) -> (venv', tenv)
                        | otherwise -> error "result type not matched"
                    where
                        venv' = S.enter venv name (E.FunEntry (map snd params') T.UNIT)
                        venv'' = foldr enterparam venv' params'
            where
                params' = map transparam params
                transparam :: A.Field -> (S.Symbol, T.Ty)
                transparam (A.Field name _ typ pos) = case S.look tenv typ of
                        Just t -> (name, t)
                        Nothing -> error ""
                enterparam :: (S.Symbol, T.Ty) -> VEnv -> VEnv
                enterparam (name, ty') venv = S.enter venv name (E.VarEntry ty')
transDec (venv, tenv, A.VarDec name _ typ init pos) = (S.enter venv name (E.VarEntry ty), tenv)
    where
        ExpTy exp ty = transExp (venv, tenv, init)
transDec (venv, tenv, A.TypeDec name ty pos) = (venv, S.enter tenv name (transTy (tenv, ty)))

transTy :: (TEnv, A.Ty) -> T.Ty
transTy (tenv, A.NameTy sym pos) = error ""
transTy (tenv, A.RecordTy fields) = error ""
transTy (tenv, A.ArrayTy sym pos) = error ""