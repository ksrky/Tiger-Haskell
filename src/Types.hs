{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Types where

import qualified Symbol

data Ty
        = INT
        | STRING
        | RECORD [(Symbol.Symbol, Ty)]
        | ARRAY Ty
        | NIL
        | UNIT
        | NAME Symbol.Symbol (Maybe Ty)
        deriving (Eq, Show)
