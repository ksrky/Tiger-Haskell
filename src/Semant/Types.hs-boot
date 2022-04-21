module Semant.Types where

import Semant.Symbol (Symbol)
import qualified Syntax.Absyn as Absyn

data Ty
        = INT
        | STRING
        | RECORD Symbol [(Symbol, Ty)]
        | ARRAY Symbol Ty
        | NIL
        | UNIT
        | NAME Symbol
        | Ref Symbol -- ref type of record and array
        | Temp Absyn.Ty