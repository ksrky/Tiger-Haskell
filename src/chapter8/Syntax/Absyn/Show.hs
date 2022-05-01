module Syntax.Absyn.Show where

import {-# SOURCE #-} Syntax.Absyn (Pos (Pos))

instance Show Pos where
        show (Pos l c) = show l ++ ":" ++ show c