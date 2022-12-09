module Temp where

import Symbol

type Temp = Int
type Label = Symbol

data TempState = TS {temps :: Temp, labs :: Int}

initState :: TempState
initState = TS{temps = 0, labs = 0}

newTemp :: TempState -> (Temp, TempState)
newTemp (TS t l) = (t, TS (t + 1) l)

makeString :: Int -> String
makeString t = "t" ++ show t

newLabel :: TempState -> (Label, TempState)
newLabel (TS t l) = ("L" ++ show l, TS t (l + 1))

namedLabel :: String -> Label
namedLabel =  symbol