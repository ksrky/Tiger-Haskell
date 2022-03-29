module Frame.MIPSFrame where

import qualified Frame
import qualified Temp

data Frame = Frame
        { name :: Temp.Label
        , formals :: [Frame.Access]
        , locals :: [Frame.Access]
        }
        deriving (Eq, Show)

instance Frame.FrameBase Frame where
        newFrame = undefined
        name = undefined
        formals = undefined
        allocLocal = undefined