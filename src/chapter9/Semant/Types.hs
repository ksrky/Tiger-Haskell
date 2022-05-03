module Semant.Types where

import Common.Symbol (Symbol)
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
        deriving (Eq)

instance Show Ty where
        show INT = "int"
        show STRING = "string"
        show (RECORD sym field) = sym ++ "{" ++ showField field ++ "}"
        show (ARRAY sym ty) = sym ++ "[" ++ show ty ++ "]"
        show NIL = "nil"
        show UNIT = "unit"
        show (NAME name) = name
        show (Ref name) = name
        show (Temp _) = undefined

showField :: [(Symbol, Ty)] -> String
showField [] = ""
showField [(sym, ty)] = sym ++ ":" ++ show ty
showField ((sym, ty) : fs) = sym ++ ":" ++ show ty ++ ", " ++ showField fs
