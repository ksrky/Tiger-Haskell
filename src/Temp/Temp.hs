module Temp.Temp where

import qualified Semant.Symbol as Symbol

import Control.Monad.State (MonadState (state), State)

type Temp = Int
type Label = Symbol.Symbol

data TempState = TS {temps :: Temp, labs :: Int}

initState :: TempState
initState = TS{temps = 0, labs = 0}

newTemp :: State TempState Temp
newTemp = state (\(TS t l) -> (t, TS (t + 1) l))

makeString :: Int -> String
makeString t = "t" ++ show t

newLabel :: State TempState Label
newLabel = state (\(TS t l) -> ("L" ++ show l, TS t (l + 1)))

namedLabel :: String -> Label
namedLabel = Symbol.symbol
