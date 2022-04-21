module Syntax.Absyn.Show where

import {-# SOURCE #-} Syntax.Absyn

instance Show Pos where
        show (Pos l c) = ":" ++ show l ++ ":" ++ show c