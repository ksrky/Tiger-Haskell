module Frame where

import Control.Monad.State

import Temp

data Access = InFrame Int | InReg Temp deriving (Eq, Show)

class FrameBase f where
        newFrame :: MonadIO m => TempState -> Label -> [Bool] -> m f
        name :: f -> Label
        formals :: f -> [Access]
        locals :: f -> [Access]
        allocLocal :: MonadIO m => TempState -> f -> Bool -> m Access