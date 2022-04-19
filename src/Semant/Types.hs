module Semant.Types where

import Semant.Symbol (Symbol)
import qualified Syntax.Absyn as Absyn

data TyCon = INT | STRING | NIL | UNIT | NAME Symbol deriving (Eq, Show)

data Ty
        = TCon TyCon
        | RECORD Symbol [(Symbol, Ty)]
        | ARRAY Symbol Ty
        | Ref Absyn.Ty
        deriving (Eq)

instance Show Ty where
        show (TCon INT) = "int"
        show (TCon STRING) = "string"
        show (RECORD sym field) = sym ++ "{" ++ showField field ++ "}"
        show (ARRAY sym ty) = sym ++ "[" ++ show ty ++ "]"
        show (TCon NIL) = "nil"
        show (TCon UNIT) = "unit"
        show (TCon (NAME name)) = name
        show (Ref sym) = "ref fail"

showField :: [(Symbol, Ty)] -> String
showField [] = ""
showField [(sym, ty)] = sym ++ ":" ++ show ty
showField ((sym, ty) : fs) = sym ++ ":" ++ show ty ++ ", " ++ showField fs