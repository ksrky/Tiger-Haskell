module Frame.X64Frame where

import qualified Common.Temp as Temp
import qualified Frame.Frame as Frame
import qualified IR.Tree as T

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
        rv = undefined

newFrame :: Monad m => Temp.Label -> [Bool] -> StateT Temp.TempState m Frame
newFrame lab escs = do
        fmls <- mapM calcformals (zip escs [0 ..])
        gets (Frame lab fmls [] . Temp.temps)
    where
        calcformals :: Monad m => (Bool, Int) -> StateT Temp.TempState m Frame.Access
        calcformals (True, n) = return (Frame.InFrame (- n))
        calcformals (False, _) = Frame.InReg <$> Temp.newTemp

allocLocal :: Monad m => Frame -> Bool -> StateT Temp.TempState m Frame
allocLocal frm True = do
        let locs = locals frm
            loc = Frame.InFrame (- length locs)
        return frm{locals = locs ++ [loc]}
allocLocal frm False = do
        m <- Temp.newTemp
        let locs = locals frm
            loc = Frame.InReg m
        return frm{locals = locs ++ [loc]}
