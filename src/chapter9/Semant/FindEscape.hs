module Semant.FindEscape where

import qualified Common.Symbol as S
import qualified Syntax.Absyn as A

import Control.Monad.State

type Depth = Int
type EscEnv = S.Table (Depth, Bool)

findEscape :: A.Exp -> A.Exp
findEscape exp = evalState (traverseExp 0 exp) S.empty

traverseVar :: Depth -> A.Var -> State EscEnv A.Var
traverseVar dep var = do
        eenv <- get
        case var of
                A.SimpleVar sym _ -> case S.look eenv sym of
                        Just (d, r)
                                | d > dep -> do
                                        S.enter sym (d, True)
                                        return var
                                | otherwise -> return var
                        Nothing -> return var -- error
                A.FieldVar v sym p -> do
                        var' <- traverseVar dep v
                        return $ A.FieldVar var' sym p
                A.SubscriptVar v e p -> do
                        traverseExp dep e
                        var' <- traverseVar dep v
                        return $ A.SubscriptVar var' e p

traverseExp :: Depth -> A.Exp -> State EscEnv A.Exp
traverseExp dep = traExp
    where
        traExp :: A.Exp -> State EscEnv A.Exp
        traExp (A.VarExp var) = do
                var' <- traverseVar dep var
                return $ A.VarExp var'
        traExp (A.CallExp s es pos) = do
                es' <- mapM (traverseExp dep) es
                return $ A.CallExp s es' pos
        traExp (A.RecordExp fields typ pos) = do
                fields' <- mapM traverserecord fields
                return $ A.RecordExp fields typ pos
            where
                traverserecord :: (S.Symbol, A.Exp, A.Pos) -> State EscEnv (S.Symbol, A.Exp, A.Pos)
                traverserecord (s, e, p) = do
                        e' <- traverseExp dep e
                        return (s, e', p)
        traExp (A.SeqExp es pos) = do
                es' <- mapM (traverseExp dep) es
                return $ A.SeqExp es' pos
        traExp (A.AssignExp var e pos) = do
                var' <- traverseVar dep var
                e' <- traverseExp dep e
                return $ A.AssignExp var' e' pos
        traExp (A.IfExp test then' melse pos) = do
                test' <- traExp test
                then'' <- traExp then'
                case melse of
                        Just else' -> do
                                else'' <- traExp else'
                                return $ A.IfExp test' then'' (Just else'') pos
                        Nothing -> return $ A.IfExp test' then'' Nothing pos
        traExp (A.WhileExp test body pos) = do
                test' <- traExp test
                body' <- traExp body
                return $ A.WhileExp test' body' pos
        traExp (A.ForExp name esc lo hi body pos) = do
                S.enter name (dep, False)
                lo' <- traExp lo
                hi' <- traExp hi
                body' <- traExp body
                eenv' <- get
                let esc' = case S.look eenv' name of
                        Just (d, e) -> e
                        Nothing -> True -- error
                return $ A.ForExp name esc' lo' hi' body' pos
        traExp (A.LetExp decs body pos) = do
                eenv <- get
                decs' <- traverseDecs (dep + 1) decs
                body' <- traExp body
                put eenv
                return $ A.LetExp decs' body' pos
        traExp (A.ArrayExp typ size init pos) = do
                init' <- traExp init
                return $ A.ArrayExp typ size init' pos
        traExp exp = return exp

traverseDecs :: Depth -> [A.Dec] -> State EscEnv [A.Dec]
traverseDecs dep = mapM (traverseDec (dep + 1))

traverseDec :: Depth -> A.Dec -> State EscEnv A.Dec
traverseDec dep fundec@(A.FunDec _ para _ body _) = do
        eenv <- get
        let dep' = dep + 1
        para' <- mapM (traverseParam dep') para
        body' <- traverseExp dep' body
        put eenv
        return fundec{A.params = para', A.decBody = body'}
traverseDec dep vardec@(A.VarDec name esc _ init _) = do
        S.enter name (dep, False)
        init' <- traverseExp dep init
        return vardec{A.decEscape = False, A.decInit = init}
traverseDec dep dec = return dec

traverseParam :: Depth -> A.Field -> State EscEnv A.Field
traverseParam dep field@(A.Field name esc _ _) = do
        S.enter name (dep, False)
        return field{A.fieldName = name, A.fieldEscape = False}
