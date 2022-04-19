module Semant.Types.Utils where

import Semant.Symbol
import Semant.Types

int :: Ty
int = TCon INT

string :: Ty
string = TCon STRING

nil :: Ty
nil = TCon NIL

unit :: Ty
unit = TCon UNIT

name :: Symbol -> Ty
name = TCon . NAME
