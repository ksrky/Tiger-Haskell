module Frame.Frame where

import qualified Temp.Temp as Temp

import Control.Monad.State

data Access = InFrame Int | InReg Temp.Temp deriving (Eq, Show)

class FrameBase f where
        newFrame :: Temp.Label -> [Bool] -> State Temp.TempState f
        name :: f -> Temp.Label
        formals :: f -> [Access]
        locals :: f -> [Access]
        allocLocal :: Bool -> f -> State Temp.TempState f
        fp :: f -> Temp.Temp
