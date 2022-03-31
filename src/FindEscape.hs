module FindEscape where

import qualified Absyn as A
import Symbol

type Depth = Int
type EscEnv = Symbol.Table (Depth, Bool)

findEscape :: A.Exp -> (EscEnv, A.Exp)
findEscape exp = traverseExp (Symbol.new [], 0, exp)

traverseVar :: (EscEnv, Depth, A.Var) -> (EscEnv, A.Var)
traverseVar (eenv, d, A.SimpleVar s p) = undefined
traverseVar (eenv, d, A.FieldVar v s p) = undefined
traverseVar (eenv, d, A.SubscriptVar v s p) = undefined

traverseExp :: (EscEnv, Depth, A.Exp) -> (EscEnv, A.Exp)
traverseExp (eenv, d, exp) = case exp of
        A.VarExp v -> (eenv', A.VarExp v')
            where
                (eenv', v') = traverseVar (eenv, d, v)
        A.NilExp -> (eenv, exp)
        A.IntExp _ -> (eenv, exp)
        A.StringExp _ -> (eenv, exp)
        A.CallExp{} -> undefined
        A.OpExp{} -> (eenv, exp)
        A.RecordExp{} -> undefined
        A.SeqExp _ -> undefined
        _ -> undefined

traverseDecs :: (EscEnv, Depth, [A.Dec]) -> EscEnv
traverseDecs = undefined
