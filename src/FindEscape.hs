module FindEscape where

import qualified Absyn as A
import qualified Symbol as S

type Depth = Int
type EscEnv = S.Table (Depth, Bool)

findEscape :: A.Exp -> (EscEnv, A.Exp)
findEscape exp = traverseExp (S.new [], 0, exp)

traverseVar :: (EscEnv, Depth, A.Var) -> (EscEnv, A.Var)
traverseVar (eenv, dep, var) = case var of
        A.SimpleVar sym _ -> case S.look eenv sym of
                Just (d, r)
                        | d > dep -> (S.enter eenv sym (dep, True), var)
                        | otherwise -> (eenv, var)
                Nothing -> (eenv, var) -- error: variable is out of range
        A.FieldVar v sym p -> (eenv', v')
            where
                (eenv', v') = traverseVar (eenv, dep, v)
        A.SubscriptVar v e p -> (eenv'', v')
            where
                (eenv'', e'') = traverseExp (eenv, dep, e)
                (eenv', v') = traverseVar (eenv, dep, v)

traverseExp :: (EscEnv, Depth, A.Exp) -> (EscEnv, A.Exp)
traverseExp (eenv, d, exp) = case exp of
        A.VarExp v -> (eenv', A.VarExp v')
            where
                (eenv', v') = traverseVar (eenv, d, v)
        A.NilExp -> (eenv, exp)
        A.IntExp _ -> (eenv, exp)
        A.StringExp _ -> (eenv, exp)
        A.CallExp s es p -> undefined
        A.OpExp{} -> (eenv, exp)
        A.RecordExp{} -> undefined
        A.SeqExp _ -> undefined
        _ -> undefined

traverseDecs :: (EscEnv, Depth, [A.Dec]) -> EscEnv
traverseDecs = undefined
