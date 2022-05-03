module Semant.Translate where

import qualified Common.Temp as Temp
import qualified Frame.Frame as Frame
import qualified Frame.X64Frame as X64Frame
import qualified IR.Tree as T

import Control.Monad.State

data Level
        = Level
                { parent :: Level
                , name :: Temp.Label
                , formals :: [Bool]
                , frame :: X64Frame.Frame
                }
        | Outermost
        deriving (Eq, Show)

data Access = Access {level :: Level, access :: Frame.Access} deriving (Eq, Show)

newLevel :: Level -> [Bool] -> State Temp.TempState Level
newLevel par fmls = do
        lab <- Temp.newLabel
        frm <- Frame.newFrame lab fmls
        return (Level par lab (True : fmls) frm) -- ?

allocLocal :: Level -> Bool -> State Temp.TempState Access
allocLocal lev@Level{frame = frm} esc = do
        frm' <- Frame.allocLocal frm esc
        return $ Access lev{frame = frm'} (last $ Frame.locals frm')
allocLocal Outermost _ = undefined

procEntryExit :: (Level, Exp) -> ()
procEntryExit = undefined

getResult :: () -> [Frame.Frag f]
getResult = undefined

data Exp
        = Ex T.Exp
        | Nx T.Stm
        | Cx ((Temp.Label, Temp.Label) -> T.Stm)

unEx :: Exp -> State Temp.TempState T.Exp
unEx (Ex exp) = return exp
unEx (Nx stm) = return $ T.ESEQ stm (T.CONST 0)
unEx (Cx genstm) = do
        r <- Temp.newTemp
        t <- Temp.newLabel
        f <- Temp.newLabel
        return $
                T.ESEQ
                        ( mkseq
                                [ T.MOVE (T.TEMP r) (T.CONST 1)
                                , genstm (t, f)
                                , T.LABEL f
                                , T.MOVE (T.TEMP r) (T.CONST 0)
                                , T.LABEL t
                                ]
                        )
                        (T.TEMP r)

unNx :: Exp -> State Temp.TempState T.Stm
unNx (Ex exp) = return $ T.EXP exp
unNx (Nx stm) = return stm
unNx (Cx genstm) = do
        t <- Temp.newLabel
        return $
                mkseq
                        [ genstm (t, t)
                        , T.LABEL t
                        ]

unCx :: Exp -> ((Temp.Label, Temp.Label) -> T.Stm)
unCx (Ex exp) = uncurry (T.CJUMP T.NE exp (T.CONST 0))
unCx (Nx _) = undefined
unCx (Cx genstm) = genstm

mkseq :: [T.Stm] -> T.Stm
mkseq [] = T.EXP $ T.CONST 0
mkseq [stm] = stm
mkseq (stm : stms) = T.SEQ stm (mkseq stms)

calcStaticLink :: Level -> T.Exp -> T.Exp
calcStaticLink lev e = T.MEM (T.BINOP T.PLUS e (T.CONST (par - chi)))
    where
        chi = Frame.fp $ frame lev
        par = Frame.fp $ frame $ parent lev

simpleVar :: (Access, Level) -> Exp
simpleVar (Access lev_dec acs, lev_use) = Ex $ Frame.exp acs (Frame.exp acs expr)
    where
        expr = walkStaticLink lev_dec lev_use (T.TEMP $ Frame.fp $ frame lev_use)
        walkStaticLink :: Level -> Level -> T.Exp -> T.Exp
        -- walkStaticLink hi Outermost e = undefined
        walkStaticLink hi lo e
                | hi == lo = e
                | otherwise = walkStaticLink hi (parent lo) (calcStaticLink lo e)

fieldVar :: Exp -> Exp -> State Temp.TempState Exp --tmp
fieldVar var rcd = do
        var' <- unEx var
        rcd' <- unEx rcd
        callExp (Temp.namedLabel "getItem") [var, rcd]

subscriptVar :: Exp -> Exp -> State Temp.TempState Exp --tmp
subscriptVar var idx = do
        var' <- unEx var
        idx' <- unEx idx
        callExp (Temp.namedLabel "getItem") [var, idx] -- Ex $ T.ESEQ (T.MOVE (T.TEMP r) y) (T.TEMP r)

nilExp :: Exp
nilExp = Ex $ T.CONST 0

intExp :: Int -> Exp
intExp i = Ex $ T.CONST i

stringExp :: String -> State Temp.TempState (Exp, Frame.Frag f)
stringExp s = do
        lab <- Temp.newLabel
        return (Ex $ T.NAME lab, Frame.STRING lab s)

callExp :: Temp.Label -> [Exp] -> State Temp.TempState Exp
callExp f es = do
        args <- mapM unEx es
        return $ Ex $ T.CALL (T.NAME f) args

binOp :: T.BinOp -> Exp -> Exp -> State Temp.TempState Exp
binOp op left right = do
        left' <- unEx left
        right' <- unEx right
        return $ Ex $ T.BINOP op left' right'

plusOp :: Exp -> Exp -> State Temp.TempState Exp
plusOp = binOp T.PLUS

minusOp :: Exp -> Exp -> State Temp.TempState Exp
minusOp = binOp T.MINUS

timesOp :: Exp -> Exp -> State Temp.TempState Exp
timesOp = binOp T.MUL

divideOp :: Exp -> Exp -> State Temp.TempState Exp
divideOp = binOp T.DIV

relOp :: T.Relop -> Exp -> Exp -> State Temp.TempState Exp
relOp op left right = do
        left' <- unEx left
        right' <- unEx right
        return $ Cx (uncurry (T.CJUMP op left' right'))

ltOp :: Exp -> Exp -> State Temp.TempState Exp
ltOp = relOp T.LT

gtOp :: Exp -> Exp -> State Temp.TempState Exp
gtOp = relOp T.GT

leOp :: Exp -> Exp -> State Temp.TempState Exp
leOp = relOp T.LE

geOp :: Exp -> Exp -> State Temp.TempState Exp
geOp = relOp T.GE

eqOp :: Exp -> Exp -> State Temp.TempState Exp
eqOp = relOp T.EQ

neqOp :: Exp -> Exp -> State Temp.TempState Exp
neqOp = relOp T.NE

recordExp :: [Exp] -> State Temp.TempState Exp
recordExp cs = callExp (Temp.namedLabel "initRecord") (Ex (T.CONST $ length cs) : cs)

seqExp :: [Exp] -> State Temp.TempState Exp
seqExp [] = return $ Nx $ T.EXP $ T.CONST 0
seqExp exps = do
        e <- unEx (last exps)
        ss <- mapM unNx (init exps)
        return $ case ss of
                [] -> Ex e
                _ -> Ex $ T.ESEQ (mkseq ss) e

assignExp :: Exp -> Exp -> State Temp.TempState Exp
assignExp left right = do
        left' <- unEx left
        right' <- unEx right
        return $ Nx $ T.MOVE left' right'

ifExp :: Exp -> Exp -> Exp -> State Temp.TempState Exp
ifExp test then' else' = do
        let genstm = unCx test
        then'' <- unEx then'
        else'' <- unEx else'
        r <- Temp.newTemp
        t <- Temp.newLabel
        f <- Temp.newLabel
        j <- Temp.newLabel
        return $
                Ex $
                        T.ESEQ
                                ( mkseq
                                        [ genstm (t, f)
                                        , T.LABEL t
                                        , T.MOVE (T.TEMP r) then''
                                        , T.JUMP (T.NAME j) [j]
                                        , T.LABEL f
                                        , T.MOVE (T.TEMP r) else''
                                        , T.LABEL j
                                        ]
                                )
                                (T.TEMP r)

whileExp :: Exp -> Exp -> State Temp.TempState Exp --tmp: break nasi
whileExp test body = do
        let genstm = unCx test
        body' <- unNx body
        lab <- Temp.newLabel
        return $
                Nx $
                        mkseq
                                [ T.LABEL lab
                                , body'
                                , T.JUMP (T.NAME lab) [lab]
                                ]

breakExp :: Temp.Label -> Exp
breakExp brkdest = Nx $ T.JUMP (T.NAME brkdest) [brkdest]

letExp :: [Exp] -> Exp -> State Temp.TempState Exp
letExp es body = do
        ss <- mapM unNx es
        e <- unEx body
        return $ Ex $ T.ESEQ (mkseq ss) e

arrayExp :: Exp -> Exp -> State Temp.TempState Exp
arrayExp size init = callExp (Temp.namedLabel "initArray") [size, init]
