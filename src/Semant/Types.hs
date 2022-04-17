module Semant.Types where

import Semant.Symbol (Symbol)

data Ty
        = INT
        | STRING
        | RECORD Symbol [(Symbol, Ty)]
        | ARRAY Symbol Ty
        | NIL
        | UNIT
        | NAME {name :: Symbol, typ :: Maybe Symbol}
        deriving (Eq, Show)
