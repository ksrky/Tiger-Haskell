module Translate where

import qualified Frame
import qualified Frame.MIPSFrame as MIPSFrame
import qualified Temp

type Exp = ()

data Level
        = Level
                { parent :: Level
                , name :: Temp.Label
                , formasls :: [Bool]
                , frame :: MIPSFrame.Frame
                }
        | Outermost
        deriving (Eq, Show)

data Access = Access {level :: Level, access :: Frame.Access} deriving (Eq, Show)

newLevel :: Level -> [Bool] -> Temp.State -> (Level, Temp.State)
newLevel par fmls s = (Level par lab fmls frm, s')
    where
        (lab, s') = Temp.newLabel s
        frm = Frame.newFrame lab fmls

allocLocal :: Level -> Bool -> Access
allocLocal lev@Level{frame = frm} esc = Access lev acs
    where
        acs = Frame.allocLocal frm esc
allocLocal Outermost _ = error ""
