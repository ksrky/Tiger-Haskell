module Compiler.Assem where

import qualified Temp.Temp as Temp

data Instr
        = OPER {assem :: String, dst :: [Temp.Temp], src :: [Temp.Temp], jump :: Maybe Temp.Label}
        | LABEL {assem :: String, lab :: Temp.Label}
        | MOVE {assem :: String, dst' :: Temp.Temp, src' :: Temp.Temp}

format :: Temp.Temp -> String -> Instr -> string
format = error "not implemented"
