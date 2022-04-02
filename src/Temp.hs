module Temp where

import qualified Symbol

newtype ST s a = ST (s -> (a, s))

app :: ST s a -> s -> (a, s)
app (ST st) = st

data State = State {temps :: Int, labs :: Int}

initState :: State
initState = State{temps = 0, labs = 0}

type Temp = Int
type Label = Symbol.Symbol

newTemp :: ST State Temp
newTemp = ST (\(State t l) -> (t, State (t + 1) l))

makeString :: Int -> String
makeString t = "t" ++ show t

newLabel :: ST State Label
newLabel = ST (\(State t l) -> ("L" ++ show l, State t (l + 1)))

namedLabel :: String -> Label
namedLabel = Symbol.symbol
