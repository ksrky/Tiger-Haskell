module Syntax.Absyn.Utils where

import Syntax.Absyn

data OpKind = Arith | Order | Equal

opkind :: Oper -> OpKind
opkind PlusOp = Arith
opkind MinusOp = Arith
opkind TimesOp = Arith
opkind DivideOp = Arith
opkind EqOp = Equal
opkind NeqOp = Equal
opkind LtOp = Order
opkind LeOp = Order
opkind GtOp = Order
opkind GeOp = Order

forToLet :: (Symbol, Bool, Exp, Exp, Exp, Pos) -> Exp
forToLet (name, esc, lo, hi, body, pos) = LetExp decs body' pos
    where
        decs =
                [ VarDec name esc (Just ("int", pos)) lo pos
                , VarDec "_limits" esc (Just ("int", pos)) hi pos
                ]
        var = SimpleVar name pos
        exp = OpExp (VarExp var) PlusOp (IntExp 1) pos
        body' = SeqExp (body : [AssignExp var exp pos]) pos