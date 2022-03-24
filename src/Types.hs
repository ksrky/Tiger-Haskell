module Types where

import qualified Symbol

type Unique = String -- haskell doesn't support 'ref', so use type names as unique values

data Ty
        = INT
        | STRING
        | RECORD [(Symbol.Symbol, Ty)] Unique
        | ARRAY Ty Unique
        | NIL
        | UNIT
        | NAME Symbol.Symbol (Maybe Ty)
        deriving (Eq, Show)
