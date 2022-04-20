module Semant.Translate where

import qualified Frame.Frame as Frame
import qualified Frame.X64Frame as X64Frame
import qualified Temp.Temp as Temp

import Control.Monad.State

data Level
        = Level
                { parent :: Level
                , name :: Temp.Label
                , formals :: [Bool]
                , frame :: X64Frame.Frame
                }
        | Outermost
        deriving (Eq, Show)

data Access = Access {level :: Level, access :: Frame.Access} deriving (Eq, Show)

newLevel :: Level -> [Bool] -> State Temp.TempState Level
newLevel par fmls = do
        lab <- state Temp.newLabel
        frm <- Frame.newFrame lab fmls
        return (Level par lab (True : fmls) frm) -- ?

allocLocal :: Level -> Bool -> State Temp.TempState Access
allocLocal lev@Level{frame = frm} esc = do
        frm' <- Frame.allocLocal esc frm
        return $ Access lev{frame = frm'} (last $ Frame.locals frm')
allocLocal Outermost _ = undefined