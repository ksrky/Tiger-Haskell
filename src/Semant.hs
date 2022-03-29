module Semant where

import qualified Absyn as A
import qualified Env as E
import qualified Symbol as S
import qualified Temp
import qualified Translate as TL
import qualified Types as T

type VEnv = S.Table E.EnvEntry
type TEnv = S.Table T.Ty

data ExpTy = ExpTy {exp :: TL.Exp, ty :: T.Ty} deriving (Show)

data ST = ST {venv :: VEnv, tenv :: TEnv, level :: TL.Level, state :: Temp.State}

getTy :: (ExpTy, TL.Level, Temp.State) -> T.Ty
getTy (ExpTy _ ty, _, _) = ty

--dirty
fst3 :: (a, b, c) -> a
fst3 (x, y, z) = x

actualTy :: T.Ty -> Maybe T.Ty
actualTy ty = case ty of
        T.NAME _ mty -> actualTy =<< mty
        T.ARRAY ty' u -> T.ARRAY <$> actualTy ty' <*> Just u
        _ -> Just ty

isNil :: T.Ty -> Bool
isNil ty = case actualTy ty of
        Just t -> T.NIL == t
        Nothing -> False

(@@) :: T.Ty -> T.Ty -> Bool
(@@) lty rty = actualTy lty == actualTy rty || isNil lty || isNil rty -- warning: Nothing == Nothing

transVar :: ST -> A.Var -> ExpTy
transVar st@(ST venv _ _ _) = trvar
    where
        trvar :: A.Var -> ExpTy
        trvar (A.SimpleVar id pos) = case S.look venv id of
                Nothing -> error $ show pos ++ "undefined variable: " ++ id
                Just (E.VarEntry _ ty) -> ExpTy () ty
                Just (E.FunEntry{}) -> error $ show pos ++ "not a variable: " ++ id
        trvar (A.FieldVar v id pos) = case ty $ trvar v of
                T.RECORD fs _ -> case lookup id fs of
                        Nothing -> error $ show pos ++ "field not found: " ++ id
                        Just ty -> ExpTy () ty
                ty -> error $ show pos ++ "not a record: " ++ show ty
        trvar (A.SubscriptVar v exp pos) = case ty $ trvar v of
                T.ARRAY ty' _
                        | ty (transExp st exp) @@ ty' -> ExpTy () ty'
                        | otherwise -> error $ show pos ++ "type not matched"
                ty' -> error $ show pos ++ "not an array: " ++ show ty'

transExp :: ST -> A.Exp -> ExpTy
transExp st@(ST venv tenv _ _) exp = trexp exp
    where
        trexp :: A.Exp -> ExpTy
        trexp (A.VarExp v) = transVar st v
        trexp A.NilExp = ExpTy () T.NIL
        trexp (A.IntExp _) = ExpTy () T.INT
        trexp (A.StringExp _) = ExpTy () T.STRING
        trexp (A.CallExp func args pos) = case S.look venv func of
                Nothing -> error $ show pos ++ "undefined function: " ++ func
                Just (E.VarEntry _ ty) -> error $ show pos ++ "not a function (in actualTy): " ++ func
                Just (E.FunEntry _ _ formals result)
                        | length args /= length formals -> error $ show pos ++ "wrong number of arguments"
                        | checkFormals args formals -> case actualTy result of
                                Just ty -> ExpTy () ty
                                Nothing -> error $ show pos ++ "result type not found: " ++ show result
                        | otherwise -> error $ show pos ++ "argument type not matched"
                    where
                        checkFormals :: [A.Exp] -> [T.Ty] -> Bool
                        checkFormals [] [] = True
                        checkFormals (a : as) (t : ts) = t @@ ty (trexp a) && checkFormals as ts
                        checkFormals _ _ = error $ show pos ++ "wrong number of arguments" -- unreachable
        trexp (A.OpExp left op right pos)
                | checkInt left && checkInt right = ExpTy () T.INT
                | isComp op && checkString left && checkString right = ExpTy () T.INT
                | isComp op = error $ show pos ++ "comparison of incompatible types"
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
                                                | otherwise -> error $ show p ++ "record type not matched. expected " ++ s ++ ":" ++ show t
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
                        | otherwise = trexps es
        trexp (A.AssignExp v exp pos) =
                if ty (transVar st v) @@ ty (trexp exp)
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
                                | otherwise -> error $ show pos ++ "types of then - else differ"
        trexp (A.WhileExp test body pos)
                | not $ checkInt test = error $ show pos ++ "test must be integer"
                | checkUnit body = ExpTy () T.UNIT
                | otherwise = error "body of while must be unit"
        trexp (A.ForExp name _ lo hi body pos)
                | not (checkInt lo) || not (checkInt hi) = error $ show pos ++ "lo and hi must be integer"
                | otherwise = trexp body
        trexp (A.BreakExp _) = ExpTy () T.UNIT -- todo: BreakExp must be used in WhileExp/ForExp
        trexp (A.LetExp decs body pos) = transExp st' body
            where
                st' = transDec st decs
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

transDec :: ST -> [A.Dec] -> ST
transDec st decs = trdecs (register st decs) decs

register :: ST -> [A.Dec] -> ST
register st@(ST venv tenv lev s) decs = st'
    where
        st' = regisfun st{tenv = tenv'} decs
        tenv' = registy (venv, tenv) decs

registy :: (VEnv, TEnv) -> [A.Dec] -> TEnv
registy (venv, tenv) decs = case decs of
        [] -> tenv
        A.TypeDec name ty pos : ds -> registy (venv, tenv') ds
            where
                tenv' = S.enter tenv name (T.NAME name Nothing)
        _ : ds -> registy (venv, tenv) ds

regisfun :: ST -> [A.Dec] -> ST
regisfun st@(ST venv tenv lev s) decs = case decs of
        [] -> st
        A.FunDec name params result body _ : ds -> case result of
                Just (typ, p) -> case S.look tenv typ of
                        Nothing -> error $ show p ++ "type not found: " ++ typ
                        Just result_ty -> regisfun st{venv = venv', state = s'} ds
                            where
                                venv' = S.enter venv name (E.FunEntry lev lab (map snd params') result_ty)
                Nothing -> regisfun st{venv = venv', state = s'} ds
                    where
                        venv' = S.enter venv name (E.FunEntry lev lab (map snd params') T.UNIT)
            where
                params' = map transparam params
                transparam :: A.Field -> (S.Symbol, T.Ty)
                transparam (A.Field name _ typ pos) = case S.look tenv typ of
                        Just t -> (name, t)
                        Nothing -> error $ show pos ++ "type not found: " ++ typ
                (lab, s') = Temp.newLabel s
        _ : ds -> regisfun st ds

trdecs :: ST -> [A.Dec] -> ST
trdecs st@(ST venv tenv lev s) decs = st''
    where
        st'' = transvar st' decs
        st' = transfun st{tenv = tenv'} decs
        tenv' = transty (tenv, decs)

transty :: (TEnv, [A.Dec]) -> TEnv
transty (tenv, decs) = case decs of
        [] -> tenv
        A.TypeDec name ty pos : ds -> transty (tenv', ds)
            where
                tenv' = S.enter tenv name (transTy (tenv, ty) name)
        _ : ds -> transty (tenv, ds)

transfun :: ST -> [A.Dec] -> ST
transfun st@(ST venv tenv lev s) decs = case decs of
        [] -> st
        A.FunDec name params result body pos : ds -> case result of
                Just (typ, p) -> case S.look tenv typ of
                        Nothing -> error $ show p ++ "type not found: " ++ typ -- unreachable
                        Just result_ty
                                | result_ty @@ ty (transExp st'{level = lev'} body) -> transfun st' ds
                                | otherwise -> error $ show pos ++ "result type not matched"
                Nothing
                        | T.UNIT @@ ty (transExp st'{level = lev'} body) -> transfun st' ds
                        | otherwise -> error $ show pos ++ "result type not matched"
            where
                st' = st{venv = venv', state = s'}
                (lev', s') =
                        let escs = replicate (length params) True -- note: default
                         in TL.newLevel lev escs s'
                params' = map transparam params
                transparam :: A.Field -> (S.Symbol, Bool, T.Ty)
                transparam (A.Field name esc typ pos) = case S.look tenv typ of
                        Just t -> (name, esc, t)
                        Nothing -> error $ show pos ++ "type not found: " ++ typ
                venv' = foldr enterparam venv params'
                enterparam :: (S.Symbol, Bool, T.Ty) -> VEnv -> VEnv
                enterparam (name, esc, ty) venv =
                        let acs = TL.allocLocal lev' esc
                         in S.enter venv name (E.VarEntry acs ty)
        _ : ds -> transfun st ds

transvar :: ST -> [A.Dec] -> ST
transvar st@(ST venv tenv lev s) decs = case decs of
        [] -> st
        A.VarDec name esc mtyp init pos : ds -> case mtyp of
                Just (typ, p) -> case S.look tenv typ of
                        Nothing -> error $ show p ++ "type not found: " ++ typ
                        Just t
                                | t @@ ty (transExp st init) -> transvar st{venv = venv'} ds
                                | otherwise -> error $ show pos ++ "type not matched"
                            where
                                venv' = S.enter venv name (E.VarEntry acs t)
                                acs = TL.allocLocal lev esc
                Nothing
                        | T.UNIT @@ ty (transExp st init) -> transvar st{venv = venv'} ds
                        | otherwise -> error $ show pos ++ "type missmatched"
                    where
                        venv' = S.enter venv name (E.VarEntry acs T.UNIT)
                        acs = TL.allocLocal lev esc
        _ : ds -> transvar st ds

transTy :: (TEnv, A.Ty) -> String -> T.Ty
transTy (tenv, A.NameTy typ pos) _ = case S.look tenv typ of
        Just ty -> ty -- todo: detect invalid cycle
        Nothing -> error $ show pos ++ "type not found: " ++ typ
transTy (tenv, A.RecordTy fields) str = T.RECORD (map transfield fields) (T.unique str)
    where
        transfield :: A.Field -> (S.Symbol, T.Ty)
        transfield (A.Field name _ typ pos) = case S.look tenv typ of
                Just ty -> (name, ty)
                Nothing -> error $ show pos ++ "type not found: " ++ typ
transTy (tenv, A.ArrayTy typ pos) str = case S.look tenv typ of
        Just ty -> T.ARRAY ty (T.unique str)
        Nothing -> error $ show pos ++ "type not found: " ++ typ
