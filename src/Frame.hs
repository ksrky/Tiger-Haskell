module Frame where

import qualified Temp
import qualified Tree as T

data Access = InFrame Int | InReg Temp.Temp deriving (Eq, Show)

class FrameBase f where
        newFrame :: Temp.Label -> [Bool] -> f
        name :: f -> Temp.Label
        formals :: f -> [Access]
        allocLocal :: f -> Bool -> Access
        fp :: f -> Temp.Temp

wordSize :: Int
wordSize = undefined

exp :: Access -> T.Exp -> T.Exp
exp (InFrame k) exp = T.MEM $ T.BINOP T.PLUS (T.CONST k) exp
exp (InReg t) _ = T.TEMP t