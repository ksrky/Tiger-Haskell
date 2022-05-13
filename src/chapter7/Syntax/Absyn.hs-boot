module Syntax.Absyn where

data Pos = Pos {line :: Int, col :: Int}

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