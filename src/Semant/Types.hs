module Semant.Types where

import Semant.Symbol (Symbol)

data Ty
        = INT
        | STRING
        | RECORD [(Symbol, Ty)]
        | ARRAY Ty
        | NIL
        | UNIT
        | NAME {name :: Symbol, typ :: Maybe Symbol}
        deriving (Eq, Show)
