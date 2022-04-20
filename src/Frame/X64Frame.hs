module Frame.X64Frame where

import qualified Frame.Frame as Frame
import qualified Temp.Temp as Temp

import Control.Monad.State

data Frame = Frame
        { name :: Temp.Label
        , formals :: [Frame.Access]
        , locals :: [Frame.Access]
        , fp :: Temp.Temp
        }
        deriving (Eq, Show)

instance Frame.FrameBase Frame where
        newFrame = newFrame
        name = name
        formals = formals
        locals = locals
        allocLocal = allocLocal
        fp = fp

newFrame :: Temp.Label -> [Bool] -> State Temp.TempState Frame
newFrame lab escs = do
        fmls <- mapM calcformals (zip escs [0 ..])
        gets (Frame lab fmls [] . Temp.temps)
    where
        calcformals :: (Bool, Int) -> State Temp.TempState Frame.Access
        calcformals (True, n) = return (Frame.InFrame (-n))
        calcformals (False, _) = do
                m <- state Temp.newTemp
                return (Frame.InReg m)

allocLocal :: Bool -> Frame -> State Temp.TempState Frame
allocLocal True frm = do
        let locs = locals frm
            loc = Frame.InFrame (-length locs)
        return frm{locals = locs ++ [loc]}
allocLocal False frm = do
        m <- state Temp.newTemp
        let locs = locals frm
            loc = Frame.InReg m
        return frm{locals = locs ++ [loc]}
