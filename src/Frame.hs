module Frame where

import qualified Temp

data Access = InFrame Int | InReg Int deriving (Eq, Show)

class FrameBase a where
        newFrame :: Temp.Label -> [Bool] -> a
        name :: a -> Temp.Label
        formals :: a -> [Access]
        allocLocal :: a -> Bool -> Access
