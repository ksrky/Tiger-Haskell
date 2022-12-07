module Syntax.Absyn where

import Common.Pos (Pos)
import Common.Symbol

data Var
        = SimpleVar Symbol Pos
        | FieldVar Var Symbol Pos
        | SubscriptVar Var Exp Pos
        deriving (Eq, Show)

data Exp
        = VarExp Var
        | NilExp
        | IntExp Int
        | StringExp (String, Pos)
        | CallExp {expFunc :: Symbol, args :: [Exp], expPos :: Pos}
        | OpExp {left :: Exp, oper :: Oper, right :: Exp, expPos :: Pos}
        | RecordExp {expField :: [(Symbol, Exp, Pos)], expTyp :: Symbol, expPos :: Pos}
        | SeqExp [Exp] Pos
        | AssignExp {expVar :: Var, exp :: Exp, expPos :: Pos}
        | IfExp {test :: Exp, then' :: Exp, else' :: Maybe Exp, expPos :: Pos}
        | WhileExp {test :: Exp, body :: Exp, expPos :: Pos}
        | ForExp {expName :: Symbol, expEscape :: Bool, lo :: Exp, hi :: Exp, expBody :: Exp, expPos :: Pos}
        | BreakExp Pos
        | LetExp {decs :: [Dec], expBody :: Exp, expPos :: Pos}
        | ArrayExp {expTyp :: Symbol, expSize :: Exp, expInit :: Exp, expPos :: Pos}
        deriving (Eq, Show)

data Dec
        = FunDec {decName :: Symbol, params :: [Field], result :: Maybe (Symbol, Pos), decBody :: Exp, decPos :: Pos}
        | VarDec {decName :: Symbol, decEscape :: Bool, decTyp :: Maybe (Symbol, Pos), decInit :: Exp, decPos :: Pos}
        | TypeDec {decName :: Symbol, decTy :: Ty, decPos :: Pos}
        deriving (Eq, Show)

data Ty
        = NameTy Symbol Pos
        | RecordTy [Field]
        | ArrayTy Symbol Pos
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

data Field = Field {fieldName :: Symbol, fieldEscape :: Bool, fieldTyp :: Symbol, fieldPos :: Pos} deriving (Eq, Show)
