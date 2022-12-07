module Semant.Types where

import Common.Symbol (Symbol)
import qualified Syntax.Absyn as A

data Ty
        = INT
        | STRING
        | RECORD Symbol [(Symbol, Ty)]
        | ARRAY Symbol Ty
        | NIL
        | UNIT
        | NAME Symbol
        | Ref Symbol -- ref type of record and array
        | Temp A.Ty
        deriving (Eq)

instance Show Ty where
        show INT = "int"
        show STRING = "string"
        show (RECORD sym field) = show sym ++ "{" ++ showField field ++ "}"
        show (ARRAY sym ty) = show sym ++ "[" ++ show ty ++ "]"
        show NIL = "nil"
        show UNIT = "unit"
        show (NAME name) = show name
        show (Ref name) = show name
        show (Temp _) = undefined

showField :: [(Symbol, Ty)] -> String
showField [] = ""
showField [(sym, ty)] = show sym ++ ":" ++ show ty
showField ((sym, ty) : fs) = show sym ++ ":" ++ show ty ++ ", " ++ showField fs
