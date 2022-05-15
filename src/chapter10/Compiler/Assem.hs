module Compiler.Assem where

import qualified Common.Symbol as Symbol
import qualified Common.Temp as Temp

data Instr
        = OPER {assem :: String, dst :: [Temp.Temp], src :: [Temp.Temp], jump :: Maybe [Temp.Label]}
        | LABEL {assem :: String, lab :: Temp.Label}
        | MOVE {assem :: String, dst' :: Temp.Temp, src' :: Temp.Temp}

format :: (Temp.Temp -> String) -> Instr -> String
format saytemp = format'
    where
        format' :: Instr -> String
        format' (OPER assem dst src Nothing) = speak assem dst src []
        format' (OPER assem dst src (Just j)) = speak assem dst src j
        format' (LABEL assem _) = assem
        format' (MOVE assem dst src) = speak assem [dst] [src] []
        speak :: String -> [Temp.Temp] -> [Temp.Temp] -> [Temp.Label] -> String
        speak assem dst src jump = f assem
            where
                saylab = Symbol.name
                f :: String -> String
                f ('`' : 's' : i : rest) = saytemp (src !! read [i]) ++ f rest
                f ('`' : 'd' : i : rest) = saytemp (dst !! read [i]) ++ f rest
                f ('`' : 'j' : i : rest) = saylab (jump !! read [i]) ++ f rest
                f ('`' : '`' : rest) = '`' : f rest
                f ('`' : _ : rest) = error "impossible: bad assem format"
                f (c : rest) = c : f rest
                f [] = []
