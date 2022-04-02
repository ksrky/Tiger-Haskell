module Frame.X64Frame where

import qualified Frame
import qualified Temp

data Frame = Frame
        { name :: Temp.Label
        , formals :: [Frame.Access]
        , locals :: [Frame.Access]
        , fp :: Temp.Temp
        }
        deriving (Eq, Show)

instance Frame.FrameBase Frame where
        newFrame = undefined
        name = undefined
        formals = undefined
        allocLocal = undefined
        fp = fp
