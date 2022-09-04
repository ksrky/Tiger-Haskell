module Common.Temp where

import qualified Common.Symbol as S

import Control.Monad.State
import qualified Data.Map.Strict as M

type Temp = Int
type Label = S.Symbol

data TempState = TS {temps :: Temp, labs :: Int}

emptyState :: TempState
emptyState = TS{temps = 0, labs = 0}

newTemp :: Monad m => StateT TempState m Temp
newTemp = state (\(TS t l) -> (t, TS (t + 1) l))

makeString :: Int -> String
makeString t = "t" ++ show t

newLabel :: Monad m => StateT TempState m Label
newLabel = state (\(TS t l) -> (S.symbol $ "L" ++ show l, TS t (l + 1)))

namedLabel :: String -> Label
namedLabel = S.symbol

type Table a = M.Map Temp a
