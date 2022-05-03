module Compiler.Assem where

import qualified Common.Symbol as Symbol
import qualified Common.Temp as Temp

data Instr
        = OPER {assem :: String, dst :: [Temp.Temp], src :: [Temp.Temp], jump :: Maybe Temp.Label}
        | LABEL {assem :: String, lab :: Temp.Label}
        | MOVE {assem :: String, dst' :: Temp.Temp, src' :: Temp.Temp}

format :: (Temp.Temp -> String) -> Instr -> String
format saytemp (OPER assem dst src Nothing) = error "not implemented"
format saytemp (OPER assem dst src (Just j)) = error "not implemented"
format saytemp (LABEL assem _) = error "not implemented"
format saytemp (MOVE assem dst src) = error "not implemented"
    where
        speak assem dst src jump = f assem
            where
                saylab = Symbol.name
                f ('`' : 's' : i : rest) = saytemp src !! read [i] : f rest
                f ('`' : 'd' : i : rest) = saytemp dst !! read [i] : f rest
                f ('`' : 'j' : i : rest) = saylab jump !! read [i] : f rest
                f ('`' : '`' : rest) = '`' : f rest
                f ('`' : _ : rest) = error "impossible bad assem format"
                f (c : rest) = c : f rest
                f [] = []
