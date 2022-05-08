module Compiler.Core where

import qualified Common.Symbol as S

data Var
        = SimpleVar S.Symbol
        | FieldVar Var S.Symbol
        | SubscriptVar Var Exp
        deriving (Eq, Show)

data Exp
        = VarExp Var
        | NilExp
        | IntExp Integer
        | StringExp String
        | CallExp S.Symbol [Exp]
        | OpExp Exp Oper Exp
        | RecordExp [(S.Symbol, Exp)]
        | SeqExp [Exp]
        | AssignExp Var Exp
        | IfExp Exp Exp (Maybe Exp)
        | WhileExp Exp Exp
        | LetExp [Dec] Exp
        | ArrayExp S.Symbol Exp Exp
        deriving (Eq, Show)

data Dec
        = FunDec S.Symbol [S.Symbol] (Maybe S.Symbol) Exp
        | VarDec S.Symbol (Maybe S.Symbol)
        deriving (Eq, Show)

data Oper
        = PlusOp
        | MinusOp
        | TimesOp
        | DivideOp
        | EqOp
        | NeqOp
        | LtOp
        | LeOp
        | GtOp
        | GeOp
        deriving (Eq, Show)