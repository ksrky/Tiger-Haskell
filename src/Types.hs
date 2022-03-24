module Types where

import qualified Symbol

type Unique = Int -- haskell doesn't support 'ref'

data Ty
        = INT
        | STRING
        | RECORD [(Symbol.Symbol, Ty)] Unique
        | ARRAY Ty Unique
        | NIL
        | UNIT
        | NAME Symbol.Symbol (Maybe Ty)
        deriving (Eq, Show)
