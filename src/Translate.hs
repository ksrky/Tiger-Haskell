module Translate where

import qualified Frame
import qualified Frame.MIPSFrame as MIPSFrame
import qualified Temp
import qualified Tree as T

data Level
        = Level
                { parent :: Level
                , name :: Temp.Label
                , formals :: [Bool]
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

type Exp = ()

{-}= Ex T.Exp
| Nx T.Stm
| Cx ((Temp.Label, Temp.Label) -> T.Stm)-}

unEx :: Exp -> T.Exp
unEx = undefined

{-unEx (Ex e) = e
unEx (Cx genstm)= T.ESEQ [T.MOVE (T.TEMP r) (T.CONST 1),
        genstm (t, f),
        T.LABEL f,
        T.MOVE (T.TEMP r) (T.CONST 0),
        T.LABEL t] (T.TEMP r)
        where
                r = Temp.newTemp
                t = Temp.newLabel
                f = Temp.newLabel
unEx (Nx s) = T.ESEQ s (T.CONST 0) -}

simpleVar :: (Access, Level) -> Exp
simpleVar = undefined