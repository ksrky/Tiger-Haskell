module Frame where

import Temp

data Access = InFrame Int | InReg Temp deriving (Eq, Show)

class FrameBase f where
        newFrame :: Label -> [Bool] -> f
        name :: f -> Label
        formals :: f -> [Access]
        locals :: f -> [Access]
        allocLocal :: f -> Bool -> Access