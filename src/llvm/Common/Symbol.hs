module Common.Symbol where

import Control.Monad.State (StateT, modify)
import qualified Data.Map.Strict as M
import Data.Text (Text, pack, unpack)

type Symbol = Text

type Table a = M.Map Symbol a

symbol :: String -> Symbol
symbol = pack

name :: Symbol -> String
name = unpack

new :: [(Symbol, a)] -> Table a
new = M.fromList

empty :: Table a
empty = M.empty

enter :: Monad m => Symbol -> a -> StateT (Table a) m ()
enter s v = modify $ \table -> M.insert s v table

look :: Table a -> Symbol -> Maybe a
look table s = M.lookup s table
