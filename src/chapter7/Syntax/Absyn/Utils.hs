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

p :: Pos
p = Pos (-1) (-1)

forToLet :: (Symbol, Bool, Exp, Exp, Exp, Pos) -> Exp
forToLet (name, esc, lo, hi, body, pos) = LetExp decs body'' pos
  where
    decs =
        [ VarDec name esc (Just ("int", pos)) lo p
        , VarDec "_limits" esc (Just ("int", p)) hi p
        ]
    i = SimpleVar name p
    exp = OpExp (VarExp i) PlusOp (IntExp 1) p
    body' = SeqExp (body : [AssignExp i exp p]) p
    test = OpExp (VarExp i) LeOp (VarExp (SimpleVar "_limits" p)) p
    body'' = WhileExp test body' p

mkSimpleVar :: Symbol -> Var
mkSimpleVar sym = SimpleVar sym p
