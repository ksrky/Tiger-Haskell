module Types where

import qualified Symbol

type Unique = Int -- haskell doesn't support 'ref', so use type names as unique values

unique :: String -> Int
unique str = -1 -- todo: generate unique id

data Ty
        = INT
        | STRING
        | RECORD [(Symbol.Symbol, Ty)] Unique
        | ARRAY Ty Unique
        | NIL
        | UNIT
        | NAME Symbol.Symbol (Maybe Ty)
        deriving (Eq, Show)
