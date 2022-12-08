module Syntax.Absyn where

import Data.IORef

data Pos = Pos {line :: Int, col :: Int} deriving (Eq, Show)

newtype Escape = Escape (IORef Bool) deriving (Eq)

instance Show Escape where show _ = ""

type Name = String

data Var
        = SimpleVar Pos Name
        | FieldVar Pos Var Name
        | SubscriptVar Pos Var Exp
        deriving (Eq, Show)

data Exp
        = VarExp Var
        | NilExp
        | IntExp Int
        | StringExp Pos String
        | CallExp Pos Name [Exp]
        | OpExp Pos Exp Oper Exp
        | RecordExp Pos [(Name, Exp, Pos)] Name
        | SeqExp [Exp]
        | AssignExp Pos Var Exp
        | IfExp Pos Exp Exp (Maybe Exp)
        | WhileExp Pos Exp Exp
        | ForExp Pos Name Escape Exp Exp Exp
        | BreakExp Pos
        | LetExp Pos [Dec] Exp
        | ArrayExp Pos Name Exp Exp
        deriving (Eq, Show)

data Dec
        = FunctionDec [FunDec]
        | VarDec Pos Name Escape (Maybe (Name, Pos)) Exp
        | TypeDec [(Name, Ty, Pos)]
        deriving (Eq, Show)

data Ty
        = NameTy Pos Name
        | RecordTy [Field]
        | ArrayTy Pos Name
        deriving (Eq, Show)

data Oper
        = PlusOp
        | MinusOp
        | TimesOp
        | DevideOp
        | EqOp
        | NeqOp
        | LtOp
        | LeOp
        | GtOp
        | GeOp
        deriving (Eq, Show)

data Field = Field Pos Name Escape Name deriving (Eq, Show)

data FunDec = FunDec Pos Name [Field] (Maybe (Name, Pos)) Exp deriving (Eq, Show)
