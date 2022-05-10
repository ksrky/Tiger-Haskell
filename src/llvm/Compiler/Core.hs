module Compiler.Core where

import qualified Common.Symbol as S

data Var
        = SimpleVar S.Symbol
        | FieldVar Var S.Symbol
        | SubscriptVar Var Exp
        deriving (Eq, Show)

data Exp
        = VAR Var
        | NIL
        | INT Integer
        | StringExp String
        | CALL S.Symbol [Exp]
        | BINOP BinOp Exp Exp
        | RecordExp [(S.Symbol, Exp)]
        | SeqExp [Exp]
        | AssignExp Var Exp
        | IfExp Exp Exp (Maybe Exp)
        | WhileExp Exp Exp
        | ArrayExp S.Symbol Exp Exp
        deriving (Eq, Show)

data Dec
        = FunDec S.Symbol [S.Symbol] (Maybe S.Symbol) Exp
        | VarDec S.Symbol (Maybe S.Symbol)
        deriving (Eq, Show)

data BinOp
        = PLUS
        | MINUS
        | MUL
        | DIV
        | AND
        | OR
        | LSHIFT
        | RSHIFT
        | ARSHIFT
        | XOR
        deriving (Eq, Show)