module Semant.Symbol where

import qualified Data.Map.Strict as M

type Symbol = String

symbol :: String -> Symbol
symbol = id

name :: Symbol -> String
name = id

type Table a = M.Map Symbol a

new :: [(Symbol, a)] -> Table a
new = M.fromList

empty :: Table a
empty = M.empty

enter :: Symbol -> a -> Table a -> Table a
enter = M.insert

look :: Symbol -> Table a -> Maybe a
look = M.lookup