module Syntax.Absyn where

import qualified Common.Symbol as S
import Syntax.Absyn.Show

data Pos = Pos {line :: Int, col :: Int} deriving (Eq)

data Var
        = SimpleVar S.Symbol Pos
        | FieldVar Var S.Symbol Pos
        | SubscriptVar Var Exp Pos
        deriving (Eq, Show)

data Exp
        = VarExp Var
        | NilExp
        | IntExp Int
        | StringExp (String, Pos)
        | CallExp {expFunc :: S.Symbol, args :: [Exp], expPos :: Pos}
        | OpExp {left :: Exp, oper :: Oper, right :: Exp, expPos :: Pos}
        | RecordExp {expField :: [(S.Symbol, Exp, Pos)], expTyp :: S.Symbol, expPos :: Pos}
        | SeqExp [Exp] Pos
        | AssignExp {expVar :: Var, exp :: Exp, expPos :: Pos}
        | IfExp {test :: Exp, then' :: Exp, else' :: Maybe Exp, expPos :: Pos}
        | WhileExp {test :: Exp, body :: Exp, expPos :: Pos}
        | ForExp {expName :: S.Symbol, expEscape :: Bool, lo :: Exp, hi :: Exp, expBody :: Exp, expPos :: Pos}
        | BreakExp Pos
        | LetExp {decs :: [Dec], expBody :: Exp, expPos :: Pos}
        | ArrayExp {expTyp :: S.Symbol, expSize :: Exp, expInit :: Exp, expPos :: Pos}
        deriving (Eq, Show)

data Dec
        = FunDec {decName :: S.Symbol, params :: [Field], result :: Maybe (S.Symbol, Pos), decBody :: Exp, decPos :: Pos}
        | VarDec {decName :: S.Symbol, decEscape :: Bool, decTyp :: Maybe (S.Symbol, Pos), decInit :: Exp, decPos :: Pos}
        | TypeDec {decName :: S.Symbol, decTy :: Ty, decPos :: Pos}
        deriving (Eq, Show)

data Ty
        = NameTy S.Symbol Pos
        | RecordTy [Field]
        | ArrayTy S.Symbol Pos
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

data Field = Field {fieldName :: S.Symbol, fieldEscape :: Bool, fieldTyp :: S.Symbol, fieldPos :: Pos} deriving (Eq, Show)
