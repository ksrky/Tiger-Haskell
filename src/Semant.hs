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
        T.NAME _ mty -> actualTy =<< mty
        T.ARRAY ty' u -> T.ARRAY <$> actualTy ty' <*> Just u
        _ -> Just ty

(@@) :: T.Ty -> T.Ty -> Bool
(@@) ty1 ty2 = actualTy ty1 == actualTy ty2 -- warning: Nothing == Nothing

transVar :: (VEnv, TEnv, A.Var) -> ExpTy
transVar (venv, tenv, var) = trvar var
    where
        trvar :: A.Var -> ExpTy
        trvar (A.SimpleVar id pos) = case S.look venv id of
                Nothing -> error $ show pos ++ "undefined variable: " ++ id
                Just (E.VarEntry ty) -> ExpTy () ty
                Just (E.FunEntry _ _) -> error $ show pos ++ "not a variable: " ++ id
        trvar (A.FieldVar v id pos) = case ty $ trvar v of
                T.RECORD fs _ -> case lookup id fs of
                        Nothing -> error $ show pos ++ "field not found: " ++ id
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
                | checkInt left && checkInt right = ExpTy () T.INT
                | isComp op && checkString left && checkString right = ExpTy () T.INT
                | isComp op = error $ show pos ++ "integer or string required"
                | otherwise = error $ show pos ++ "integer required"
            where
                isComp :: A.Oper -> Bool
                isComp op = op == A.EqOp || op == A.NeqOp || op == A.LtOp || op == A.LeOp || op == A.GtOp || op == A.GeOp
        trexp (A.RecordExp fields typ pos) = case S.look tenv typ of
                Nothing -> error $ show pos ++ "record type not found: " ++ typ
                Just ty' -> case actualTy ty' of
                        Nothing -> error $ show pos ++ "type not found (in actualTy): " ++ show ty'
                        Just (T.RECORD fs_ty u) -> ExpTy () (T.RECORD (transrecord fields fs_ty) u)
                            where
                                {- order of records doesn't matter -}
                                transrecord :: [(A.Symbol, A.Exp, A.Pos)] -> [(S.Symbol, T.Ty)] -> [(S.Symbol, T.Ty)]
                                transrecord _ [] = fs_ty
                                transrecord fs ((s, t) : sts) = case lookup3 s fs of
                                        Just (e, p)
                                                | ty (trexp e) @@ t -> transrecord fs sts
                                                | otherwise -> error $ show p ++ "record type not matched"
                                        Nothing -> error $ show pos ++ "record not found: " ++ s
                                lookup3 :: (Eq a) => a -> [(a, b, c)] -> Maybe (b, c)
                                lookup3 _ [] = Nothing
                                lookup3 key ((x, y, z) : xyzs)
                                        | key == x = Just (y, z)
                                        | otherwise = lookup3 key xyzs
                        _ -> error $ show pos ++ "record type required"
        trexp (A.SeqExp (seqexp, pos)) = trexps seqexp
            where
                trexps :: [A.Exp] -> ExpTy
                trexps [] = ExpTy () T.UNIT
                trexps (e : es)
                        | null es = trexp e
                        | checkUnit e = trexps es
                        | otherwise = error $ show pos ++ "unit type required except the last expression"
        trexp (A.AssignExp v exp pos) =
                if ty (transVar (venv, tenv, v)) @@ ty (trexp exp)
                        then ExpTy () T.UNIT
                        else error $ show pos ++ "var type not matched"
        trexp (A.IfExp test then' else' pos)
                | not $ checkInt test = error $ show pos ++ "test must be integer"
                | otherwise = case else' of
                        Nothing
                                | checkUnit then' -> ExpTy () T.UNIT
                                | otherwise -> error $ show pos ++ "then must be unit when else is none"
                        Just else''
                                | ty (trexp then') @@ ty (trexp else'') -> trexp then'
                                | otherwise -> error $ show pos ++ "type not matched between then and else"
        trexp (A.WhileExp test body pos)
                | not $ checkInt test = error $ show pos ++ "test must be integer"
                | otherwise = trexp body
        trexp (A.ForExp name _ lo hi body pos)
                | not (checkInt lo) || not (checkInt hi) = error $ show pos ++ "integer required"
                | otherwise = trexp body
        trexp (A.BreakExp _) = ExpTy () T.UNIT -- todo: BreakExp must be used in WhileExp/ForExp
        trexp (A.LetExp decs body pos) = transExp (venv', tenv', body)
            where
                (venv', tenv') = transDecs (venv, tenv, decs)
        trexp (A.ArrayExp typ size init pos) = case S.look tenv typ of
                Nothing -> error $ show pos ++ "type not found: " ++ typ
                Just t -> case actualTy t of
                        Nothing -> error $ show pos ++ "type not found (in actualTy): " ++ show t
                        Just (T.ARRAY t' u)
                                | not $ checkInt size -> error $ show pos ++ "array size must be integer"
                                | ty (trexp init) @@ t' -> ExpTy () (T.ARRAY t' u)
                                | otherwise -> error $ show pos ++ "type not matched"
                        _ -> error $ show pos ++ "array type required"

        checkInt :: A.Exp -> Bool
        checkInt exp = ty (trexp exp) @@ T.INT

        checkString :: A.Exp -> Bool
        checkString exp = ty (trexp exp) @@ T.STRING

        checkUnit :: A.Exp -> Bool
        checkUnit exp = ty (trexp exp) @@ T.UNIT

        checkFormals :: [A.Exp] -> [T.Ty] -> Bool
        checkFormals args formals = and $ (@@) <$> map (ty . trexp) args <*> formals

transDecs :: (VEnv, TEnv, [A.Dec]) -> (VEnv, TEnv)
transDecs (venv, tenv, []) = (venv, tenv)
transDecs (venv, tenv, dec : decs) = transDecs (venv', tenv', decs)
    where
        (venv', tenv') = transDec (venv, tenv, dec)

transDec :: (VEnv, TEnv, A.Dec) -> (VEnv, TEnv) --todo: recursive function definition
transDec (venv, tenv, A.FunctionDec fundecs) = case fundecs of
        [] -> (venv, tenv)
        A.FunDec name params result body pos : fs -> case result of
                Just (typ, p) -> case S.look tenv typ of
                        Nothing -> error $ show p ++ "type not found: " ++ typ
                        Just result_ty
                                | result_ty @@ ty (transExp (venv'', tenv, body)) -> (venv', tenv)
                                | otherwise -> error $ show pos ++ "result type not matched"
                            where
                                venv' = S.enter venv name (E.FunEntry (map snd params') result_ty)
                                venv'' = foldr enterparam venv' params'
                Nothing
                        | T.UNIT @@ ty (transExp (venv'', tenv, body)) -> (venv', tenv)
                        | otherwise -> error $ show pos ++ "result type not matched"
                    where
                        venv' = S.enter venv name (E.FunEntry (map snd params') T.UNIT)
                        venv'' = foldr enterparam venv' params'
            where
                params' = map transparam params
                transparam :: A.Field -> (S.Symbol, T.Ty)
                transparam (A.Field name _ typ pos) = case S.look tenv typ of
                        Just t -> (name, t)
                        Nothing -> error $ show pos ++ "type not found: " ++ typ
                enterparam :: (S.Symbol, T.Ty) -> VEnv -> VEnv
                enterparam (name, ty') venv = S.enter venv name (E.VarEntry ty')
transDec (venv, tenv, A.VarDec name _ typ init pos) = (S.enter venv name (E.VarEntry ty), tenv)
    where
        ExpTy exp ty = transExp (venv, tenv, init)
transDec (venv, tenv, A.TypeDec name ty pos) = (venv, S.enter tenv name (transTy (tenv, ty) name)) --todo: recursive type definition

transTy :: (TEnv, A.Ty) -> String -> T.Ty
transTy (tenv, A.NameTy typ pos) uid = case S.look tenv typ of
        Just ty -> ty
        Nothing -> error $ show pos ++ "type not found: " ++ typ
transTy (tenv, A.RecordTy fields) uid = T.RECORD (map transfield fields) uid
    where
        transfield :: A.Field -> (S.Symbol, T.Ty)
        transfield (A.Field name _ typ pos) = case S.look tenv typ of
                Just ty -> (name, ty)
                Nothing -> error $ show pos ++ "type not found: " ++ typ
transTy (tenv, A.ArrayTy typ pos) uid = case S.look tenv typ of
        Just ty -> T.ARRAY ty uid
        Nothing -> error $ show pos ++ "type not found: " ++ typ