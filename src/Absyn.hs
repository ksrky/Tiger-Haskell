module Absyn where

type Pos = (Int, Int)
type Symbol = String

data Var
        = SimpleVar Symbol Pos
        | FieldVar Var Symbol Pos
        | SubscriptVar Var Exp Pos
        deriving (Eq, Show)

data Exp
        = VarExp Var
        | NilExp
        | IntExp Integer
        | StringExp (String, Pos)
        | CallExp {funcExp :: Symbol, argsExp :: [Exp], posExp :: Pos}
        | OpExp {leftExp :: Exp, operExp :: Oper, rightExp :: Exp, posExp :: Pos}
        | RecordExp {fieldsExp :: [(Symbol, Exp, Pos)], typExp :: Symbol, posExp :: Pos}
        | SeqExp [(Exp, Pos)]
        | AssignExp {varExp :: Var, expExp :: Exp, posExp :: Pos}
        | IfExp {testExp :: Exp, thenExp :: Exp, elseExp :: Maybe Exp, posExp :: Pos}
        | WhileExp {testExp :: Exp, bodyExp :: Exp, posExp :: Pos}
        | ForExp {nameExp :: Symbol, escapeExp :: Bool, loExp :: Exp, hiExp :: Exp, bodyExp :: Exp, posExp :: Pos}
        | BreakExp Pos
        | LetExp {decsExp :: [Dec], bodyExp :: Exp, posExp :: Pos}
        | ArrayExp {typExp :: Symbol, sizeExp :: Exp, initExp :: Exp, posExp :: Pos}
        deriving (Eq, Show)

data Dec
        = FunctionDec [FunDec]
        | VarDec {nameDec :: Symbol, escapeDec :: Bool, typDec :: Maybe (Symbol, Pos), initDec :: Exp, posDec :: Pos}
        | TypeDec {nameDec :: Symbol, tyDec :: Ty, posDec :: Pos}
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
        | DevideOp
        | EqOp
        | NeqOp
        | LtOp
        | LeOp
        | GtOp
        | GeOp
        deriving (Eq, Show)

data Field = Field {nameField :: Symbol, escapeField :: Bool, typField :: Symbol, posField :: Pos} deriving (Eq, Show)

data FunDec = FunDec {nameFunDec :: Symbol, params :: [Field], result :: Maybe (Symbol, Pos), bodyFunDec :: Exp, posFunDec :: Pos} deriving (Eq, Show)
