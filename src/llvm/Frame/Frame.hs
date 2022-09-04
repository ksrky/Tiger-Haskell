module Frame.Frame where

import qualified Common.Temp as Temp
import qualified Compiler.Assem as A
import qualified IR.Tree as T

import Control.Monad.State

data Access = InFrame Int | InReg Temp.Temp deriving (Eq, Show)

class FrameBase f where
        newFrame :: Monad m => Temp.Label -> [Bool] -> StateT Temp.TempState m f
        name :: f -> Temp.Label
        formals :: f -> [Access]
        locals :: f -> [Access]
        allocLocal :: Monad m => f -> Bool -> StateT Temp.TempState m f
        fp :: f -> Temp.Temp
        rv :: f -> Temp.Temp

data Frag f
        = PROC {body :: T.Stm, frame :: f}
        | STRING Temp.Label String
        deriving (Eq, Show)

wordSize :: T.Exp
wordSize = T.CONST 32

exp :: Access -> T.Exp -> T.Exp
exp (InFrame k) e = T.MEM (T.BINOP T.PLUS e (T.CONST k))
exp (InReg t) _ = T.TEMP t

externalCall :: String -> [T.Exp] -> T.Exp
externalCall s = T.CALL (T.NAME $ Temp.namedLabel s)

procEntryExit1 :: FrameBase f => (f, T.Stm) -> T.Stm
procEntryExit1 (frame, body) = body

{-procEntryExit2 :: FrameBase f => f -> [A.Instr] -> [A.Instr]
procEntryExit2 frame body =
        body
                ++ [ A.OPER
                        { A.assem = ""
                        , A.src = [0, rv frame, fp frame] ++ calleesaves -- tmp
                        , A.dst = []
                        , A.jump = Nothing
                        }
                   ]
-}
--procEntryExit3 :: FrameBase f => f -> [A.Instr] -> b
--procEntryExit3 frame body = {prolog="PROCEDURE "++Symbol.name name ++"\n", body=body, epilog="END "++Symbol.name name ++ "\n"}