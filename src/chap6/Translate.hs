{-# OPTIONS_GHC -Wno-partial-fields #-}

module Translate where

import Control.Monad.State

import qualified Frame
import Frame.X64Frame as X64Frame
import Temp

data Level
        = Level
                { lev_parent :: Level
                , lev_name :: Temp.Label
                , lev_formals :: [Bool]
                , lev_frame :: X64Frame.Frame
                }
        | Outermost
        deriving (Eq, Show)

data Access = Access {level :: Level, access :: Frame.Access} deriving (Eq, Show)

newLevel :: MonadIO m => TempState -> Level -> [Bool] -> m Level
newLevel st par fmls = do
        lab <- newLabel st
        frm <- Frame.newFrame st lab fmls
        return (Level par lab (True : fmls) frm) -- ?: True : fmls

allocLocal :: MonadIO m => TempState -> Level -> Bool -> m Access
allocLocal st lev@Level{lev_frame = frm} esc = do
        acs <- Frame.allocLocal st frm esc
        return $ Access lev acs
allocLocal _ Outermost _ = undefined