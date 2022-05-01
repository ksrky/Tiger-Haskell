module Syntax.Absyn.Utils where

import Syntax.Absyn (Oper (..))

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
