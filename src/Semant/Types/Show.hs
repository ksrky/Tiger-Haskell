module Semant.Types.Show where

import {-# SOURCE #-} Semant.Types

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