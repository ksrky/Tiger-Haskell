module Syntax.Absyn.Show where

import {-# SOURCE #-} Syntax.Absyn (Oper (..), Pos (Pos))

instance Show Pos where
        show (Pos l c) = show l ++ ":" ++ show c

instance Show Oper where
        show PlusOp = "(+)"
        show MinusOp = "(-)"
        show TimesOp = "(*)"
        show DivideOp = "(/)"
        show EqOp = "(=)"
        show NeqOp = "(<>)"
        show LtOp = "(<)"
        show LeOp = "(<=)"
        show GtOp = "(>)"
        show GeOp = "(>=)"