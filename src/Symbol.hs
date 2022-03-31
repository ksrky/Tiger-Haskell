module Symbol where

import qualified Data.Map.Strict as M

type Symbol = String

type Table a = M.Map Symbol a

symbol :: String -> Symbol
symbol s = s

new :: [(String, a)] -> Table a
new = M.fromList

empty :: Table a
empty = M.empty

enter :: Table a -> Symbol -> a -> Table a
enter table s v = M.insert s v table

look :: Table a -> Symbol -> Maybe a
look table s = M.lookup s table
