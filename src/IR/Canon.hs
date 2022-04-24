module IR.Canon where

import qualified IR.Tree as T
import qualified Temp.Temp as Temp
import qualified Semant.Symbol as Symbol

import Control.Monad.State

linearize :: T.Stm -> State Temp.TempState [T.Stm]
linearize stm0 = do
        stm <- doStm stm0
        return $ linear (stm, [])

-- ==================== linearize =========================
infixl 0 %
(%) :: T.Stm -> T.Stm -> T.Stm
(T.EXP (T.CONST _)) % x = x
x % (T.EXP (T.CONST _)) = x
x % y = T.SEQ x y

commute :: (T.Stm, T.Exp) -> Bool
commute (T.EXP (T.CONST _), _) = True
commute (_, T.NAME _) = True
commute (_, T.CONST _) = True
commute _ = False

nop :: T.Stm
nop = T.EXP (T.CONST 0)

reorder :: [T.Exp] -> State Temp.TempState (T.Stm, [T.Exp])
reorder [] = return (nop, [])
reorder (e@(T.CALL _ _) : rest) = do
        t <- Temp.newTemp
        reorder (T.ESEQ (T.MOVE (T.TEMP t) e) (T.TEMP t) : rest)
reorder (a : rest) = do
        (stms, e) <- doExp a
        (stms', el) <- reorder rest
        if commute (stms', e)
                then return (stms % stms', e : el)
                else do
                        t <- Temp.newTemp
                        return (stms % T.MOVE (T.TEMP t) e % stms', T.TEMP t : el)

reorderStm :: [T.Exp] -> ([T.Exp] -> T.Stm) -> State Temp.TempState T.Stm
reorderStm el build = do
        (stms, el') <- reorder el
        return (stms % build el')

reorderExp :: [T.Exp] -> ([T.Exp] -> T.Exp) -> State Temp.TempState (T.Stm, T.Exp)
reorderExp el build = do
        (stms, el') <- reorder el
        return (stms, build el')

doStm :: T.Stm -> State Temp.TempState T.Stm
doStm (T.SEQ a b) = (%) <$> doStm a <*> doStm b
doStm (T.JUMP e labs) = reorderStm [e] (\[e'] -> T.JUMP e' labs)
doStm (T.CJUMP p a b t f) = reorderStm [a, b] (\[a', b'] -> T.CJUMP p a' b' t f)
doStm (T.MOVE (T.TEMP t) b) = reorderStm [b] (\[b'] -> T.MOVE (T.TEMP t) b')
doStm (T.MOVE (T.MEM e) b) = reorderStm [e, b] (\[e', b'] -> T.MOVE (T.MEM e') b')
doStm (T.MOVE (T.ESEQ s e) b) = doStm (T.SEQ s (T.MOVE e b))
doStm (T.EXP (T.CALL e el)) = reorderStm (e : el) (\(e' : el') -> T.EXP (T.CALL e' el'))
doStm (T.EXP e) = reorderStm [e] (\[e'] -> T.EXP e)
doStm s = reorderStm [] (\[] -> s)

doExp :: T.Exp -> State Temp.TempState (T.Stm, T.Exp)
doExp (T.BINOP p a b) = reorderExp [a, b] (\[a', b'] -> T.BINOP p a' b')
doExp (T.MEM a) = reorderExp [a] (\[a'] -> T.MEM a')
doExp (T.ESEQ s e) = do
        stms <- doStm s
        (stms', e) <- doExp e
        return (stms % stms', e)
doExp (T.CALL e el) = reorderExp (e : el) (\(e : el) -> T.CALL e el)
doExp e = reorderExp [] (\[] -> e)

linear :: (T.Stm, [T.Stm]) -> [T.Stm]
linear (T.SEQ a b, l) = linear (a, linear (b, l))
linear (s, l) = s : l

-- ==================== linearize =========================

type Block = [T.Stm]

basicBlocks :: [T.Stm] -> State Temp.TempState ([[T.Stm]], Temp.Label)
basicBlocks stms = do
        done <- Temp.newLabel
        let
                blocks :: ([T.Stm], [[T.Stm]]) -> State Temp.TempState [[T.Stm]]
                blocks (head@(T.LABEL _) : tail, blist) = next (tail, [head])
                        where
                                next :: ([T.Stm], [T.Stm]) -> State Temp.TempState [[T.Stm]]
                                next (s@(T.JUMP{}) : rest, thisblock) = endblock (rest, s : thisblock)
                                next (s@(T.CJUMP{}) : rest, thisblock) = endblock (rest, s : thisblock)
                                next (stms@(T.LABEL lab : _), thisblock) = next (T.JUMP (T.NAME lab) [lab] : stms, thisblock)
                                next (s : rest, thisblock) = next (rest, s : thisblock)
                                next ([], thisblock) = next ([T.JUMP (T.NAME done) [done]], thisblock)

                                endblock :: ([T.Stm], [T.Stm]) -> State Temp.TempState [[T.Stm]]
                                endblock (stms, thisblock) = blocks (stms, reverse thisblock : blist)
                blocks ([], blist) = return $ reverse blist
                blocks (stms, blist) = do
                        lab <- Temp.newLabel
                        blocks (T.LABEL lab : stms, blist)
        stms' <- blocks (stms, [])
        return (stms', done)

enterblock :: [T.Stm]-> Symbol.Table [T.Stm] -> Symbol.Table [T.Stm]
enterblock b@(T.LABEL s : _) table = Symbol.enter table s b
enterblock _ table = table

splitlast :: [a] -> ([a], a)
splitlast []= undefined
splitlast [x] = ([],x)
splitlast (h:t) = let (t',last) = splitlast t in (h:t', last)

trace :: Symbol.Table [T.Stm] -> [T.Stm] -> [[T.Stm]] -> State Temp.TempState [T.Stm]
trace table b@(T.LABEL lab:_) rest = do
        let table = Symbol.enter table lab []
        case splitlast b of
                (most, T.JUMP (T.NAME lab') _) -> case Symbol.look table lab of
                        Just b'@(_:_) -> trace table b' rest
                        _ ->  (b++) <$> getnext table rest
                (most, T.CJUMP opr x y t f) -> case (Symbol.look table t, Symbol.look table f) of
                        (_, Just b'@(_:_)) -> (b++) <$> trace table b' rest
                        (Just b'@(_:_), _) -> ((most++ [T.CJUMP (T.notRel opr) x y f t])++) <$>  trace table b' rest
                        _ -> do
                                f' <- Temp.newLabel
                                ((most ++ [T.CJUMP opr x y t f',  T.LABEL f', T.JUMP (T.NAME f) [f]])++)  <$> getnext table rest
                (most, T.JUMP{}) -> (b++) <$> getnext table rest
                _ -> error ""
trace _ _ _ = undefined

getnext :: Symbol.Table [T.Stm] ->[[T.Stm]] -> State Temp.TempState [T.Stm]
getnext table (b@(T.LABEL lab:_):rest) = case Symbol.look table lab of
        Just (_:_) -> trace table b rest
        _ -> getnext table rest
getnext table [] = return []
getnext _ _ = undefined

traceSchedule :: ([[T.Stm]], Temp.Label) -> State Temp.TempState [T.Stm]
traceSchedule (blocks, done)= (++ [T.LABEL done]) <$> getnext (foldr enterblock Symbol.empty blocks) blocks