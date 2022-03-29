module Temp where

import Symbol (Symbol, symbol)

data State = State {temps :: Int, labs :: Int}

initState :: State
initState = State{temps = 0, labs = 0}

type Temp = Int
type Label = Symbol.Symbol

newTemp :: State -> (Temp, State)
newTemp t@State{temps = temps} = (temps, t{temps = temps + 1})

makeString :: Int -> String
makeString t = "t" ++ show t

newLabel :: State -> (Label, State)
newLabel t@State{labs = labs} = (label, t{labs = labs + 1})
 where
  label = "L" ++ show labs

namedLabel :: String -> Label
namedLabel = Symbol.symbol
