module Frame.X64Frame where

import qualified Frame
import Temp

import Control.Monad.State

-- | special registers
sp, fp :: String
sp = "rsp"
fp = "rbp"

-- | registers for integer arguments
rcx, rdx, r8, r9 :: String
rcx = "rcx"
rdx = "rdx"
r8 = "r8"
r9 = "r9"

-- | other registers
r10, r11, r12, r13, r14, r15 :: String
r10 = "r10"
r11 = "r11"
r12 = "r12"
r13 = "r13"
r14 = "r14"
r15 = "r15"

-------------------------------------

data Frame = Frame
        { frm_name :: Label
        , frm_formals :: [Frame.Access]
        , frm_locals :: [Frame.Access]
        , frm_fp :: Temp
        }
        deriving (Eq, Show)

instance Frame.FrameBase Frame where
        newFrame = newFrame
        name = frm_name
        formals = frm_formals
        locals = frm_locals
        allocLocal = allocLocal

newFrame :: MonadIO m => TempState -> Label -> [Bool] -> m Frame
newFrame st lab escs = do
        fmls <- mapM calcformals (zip escs [0 ..])
        Frame lab fmls [] <$> newTemp st
    where
        calcformals :: MonadIO m => (Bool, Int) -> m Frame.Access
        calcformals (True, n) = return (Frame.InFrame (- n))
        calcformals (False, _) = Frame.InReg <$> newTemp st

allocLocal :: MonadIO m => TempState -> Frame -> Bool -> m Frame.Access
allocLocal _ frm True = do
        let locs = frm_locals frm
        return $ Frame.InFrame (- length locs)
allocLocal st _ False = Frame.InReg <$> newTemp st