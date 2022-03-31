module Frame where

import qualified Temp
import qualified Tree

data Access = InFrame Int | InReg Temp.Temp deriving (Eq, Show)

class FrameBase a where
        newFrame :: Temp.Label -> [Bool] -> a
        name :: a -> Temp.Label
        formals :: a -> [Access]
        allocLocal :: a -> Bool -> Access

fp :: Temp.Temp
fp = undefined
wordSize :: Int
wordSize = undefined
exp :: Access -> Tree.Exp -> Tree.Exp
exp = undefined