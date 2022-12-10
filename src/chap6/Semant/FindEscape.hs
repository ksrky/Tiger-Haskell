module Semant.FindEscape where

import Symbol
import Syntax.Absyn as A

import Control.Monad.Reader
import Data.IORef

type Depth = Int
data EscEnv = EscEnv {esc_depth :: Depth, esc_table :: Table (Depth, IORef Bool)}
type FindEsc m = ReaderT EscEnv m

findEscape :: MonadIO m => Exp -> m ()
findEscape exp = runReaderT (traverseExp exp) (EscEnv 0 empty)

updateEscape :: MonadIO m => Name -> FindEsc m ()
updateEscape x = do
        EscEnv dep env <- ask
        case look x env of
                Just (d, ref) | d > dep -> liftIO $ writeIORef ref True
                _ -> return ()

traverseVar :: MonadIO m => Var -> FindEsc m ()
traverseVar var = case var of
        SimpleVar _ x -> updateEscape x
        FieldVar _ var _ -> traverseVar var
        SubscriptVar _ var exp -> do
                traverseVar var
                traverseExp exp

traverseExp :: MonadIO m => Exp -> FindEsc m ()
traverseExp = traexp
    where
        traexp :: MonadIO m => Exp -> FindEsc m ()
        traexp (VarExp var) = traverseVar var
        traexp NilExp = return ()
        traexp (IntExp _) = return ()
        traexp (StringExp _ _) = return ()
        traexp (CallExp _ _ args) = mapM_ traverseExp args
        traexp (OpExp _ lhs _ rhs) = do
                traexp lhs
                traexp rhs
        traexp (RecordExp _ fields _) = mapM_ (traverseExp . \(_, e, _) -> e) fields
        traexp (SeqExp seqexp) = mapM_ traverseExp seqexp
        traexp (AssignExp _ var exp) = do
                traverseVar var
                traverseExp exp
        traexp (IfExp _ test then' melse) = do
                traexp test
                traexp then'
                mapM_ traexp melse
        traexp (WhileExp _ test body) = do
                traexp test
                traexp body
        traexp (ForExp _ i _ lo hi body) = do
                traexp lo
                traexp hi
                ref <- liftIO $ newIORef False
                local (\(EscEnv dep env) -> EscEnv dep (enter i (dep, ref) env)) $ traexp body
        traexp (BreakExp _) = return ()
        traexp (LetExp _ decs body) = do
                env' <- local (\env -> env{esc_depth = esc_depth env + 1}) $ traverseDecs decs
                local (const env') $ traexp body
        traexp (ArrayExp _ _ size init) = do
                traexp size
                traexp init

traverseDecs :: MonadIO m => [Dec] -> FindEsc m EscEnv
traverseDecs [] = ask
traverseDecs (dec : decs) = do
        EscEnv dep env <- ask
        env' <- case dec of
                TypeDec{} -> asks esc_table
                FunctionDec fundecs -> traverseFunDec fundecs
                VarDec _ name (Escape ref) _ init -> do
                        traverseExp init
                        liftIO $ writeIORef ref False
                        return $ enter name (dep, ref) env
        local (const (EscEnv dep env')) $ traverseDecs decs
    where
        traverseFunDec :: MonadIO m => [FunDec] -> FindEsc m (Table (Depth, IORef Bool))
        traverseFunDec [] = asks esc_table
        traverseFunDec ((FunDec _ _ params _ body) : fundecs) = do
                env' <- traparams params
                local (const env') $ traverseExp body
                traverseFunDec fundecs
        traparams :: MonadIO m => [Field] -> FindEsc m EscEnv
        traparams [] = ask
        traparams ((Field _ name (Escape ref) _) : params) = do
                dep <- asks esc_depth
                local (\env -> env{esc_table = enter name (dep, ref) (esc_table env)}) $ traparams params