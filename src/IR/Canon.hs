module IR.Canon where

import qualified IR.Tree as T
import qualified Temp.Temp as Temp

linearize :: T.Stm -> [T.Stm]
linearize = error ""

basicBlocks :: [T.Stm] -> ([[T.Stm]], Temp.Label)
basicBlocks = error ""

traceSchedule :: ([[T.Stm]], Temp.Label) -> [T.Stm]
traceSchedule = error ""

commute :: (T.Stm, T.Exp) -> Bool
commute (T.EXP (T.CONST _), _) = True
commute (_, T.NAME _) = True
commute (_, T.CONST _) = True
commute _ = False

reorderStm :: [T.Exp] -> ([T.Exp] -> T.Stm) -> T.Stm
reorderStm = undefined

reorderExp :: [T.Exp] -> ([T.Exp] -> T.Exp) -> (T.Stm, T.Exp)
reorderExp = undefined

doStm :: T.Stm -> T.Stm
doStm (T.JUMP e labs) = reorderStm [e] (\[e] -> T.JUMP e labs)
doStm (T.CJUMP p a b t f) = reorderStm [a, b] (\[a, b] -> T.CJUMP p a b t f)
doStm (T.MOVE (T.TEMP t) b) = reorderStm [b] (\[b] -> T.MOVE (T.TEMP t) b)
doStm (T.MOVE (T.MEM e) b) = reorderStm [e, b] (\[e, b] -> T.MOVE (T.MEM e) b)
doStm _ = error ""

doExp :: T.Exp -> (T.Stm, T.Exp)
doExp (T.BINOP p a b) = reorderExp [a, b] (\[a, b] -> T.BINOP p a b)
doExp (T.MEM a) = reorderExp [a] (\[a] -> T.MEM a)
doExp _ = error ""