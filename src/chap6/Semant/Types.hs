{-# LANGUAGE OverloadedStrings #-}

module Semant.Types where

import Data.IORef
import Prettyprinter

import Semant.Symbol (Symbol)

type Unique = Int

data Ty
        = INT
        | STRING
        | RECORD [(Symbol, Ty)] Unique
        | ARRAY Ty Unique
        | NIL
        | UNIT
        | NAME Symbol (IORef (Maybe Ty))
        deriving (Eq)

instance Pretty Ty where
        pretty INT = "int"
        pretty STRING = "string"
        pretty (RECORD fields _) =
                hsep
                        [ lbrace
                        , concatWith (surround (comma <> space)) (map (\(lab, ty) -> pretty lab <> colon <+> pretty ty) fields)
                        , rbrace
                        ]
        pretty (ARRAY ty _) = hcat [pretty ty, lbracket, rbracket]
        pretty NIL = "nil"
        pretty UNIT = "unit"
        pretty (NAME name _) = pretty name