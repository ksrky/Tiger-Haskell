module Semant.Translate where

import qualified Frame.Frame as Frame
import qualified Frame.X64Frame as X64Frame
import qualified IR.Tree as T
import qualified Temp.Temp as Temp

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

getResult :: () -> [X64Frame.Frag]
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

simpleVar :: (Access, Level) -> Exp
simpleVar (Access lev_dec acs, lev_use) = Ex $ Frame.exp acs t_exp
    where
        t_exp = followStaticLink lev_dec lev_use (T.TEMP $ Frame.fp $ frame lev_use)
        followStaticLink :: Level -> Level -> T.Exp -> T.Exp
        followStaticLink hi Outermost e = undefined
        followStaticLink hi lo e =
                if hi == lo
                        then e
                        else followStaticLink hi (parent lo) (staticLink e)

subscriptVar :: (Access, Level) -> Exp
subscriptVar (Access lev_dec acs, lev_use) = undefined

nilExp :: Exp
nilExp = Ex $ T.CONST 0

intExp :: Int -> Exp
intExp i = Ex $ T.CONST i

{-stringExp :: String -> Temp.Temp -> (Exp, Frame.Frag, Temp.Temp)
stringExp s temp =
  let
    (label, temp') = Temp.newLabel temp
    frag = Frame.STRING label s
    expr = Ex $ T.NAME label
  in
   (expr, frag, temp')-}

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

callExp :: Temp.Label -> [Exp] -> State Temp.TempState Exp
callExp f es = do
        args <- mapM unEx es
        return $ Ex $ T.CALL (T.NAME f) args

ifThenElse :: Exp -> Exp -> Exp -> State Temp.TempState Exp
ifThenElse test then' else' = do
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

recordExp :: [Exp] -> State Temp.TempState Exp
recordExp cs = callExp (Temp.namedLabel "initRecord") (Ex (T.CONST $ length cs) : cs)

arrayExp :: Exp -> Exp -> State Temp.TempState Exp
arrayExp size init = callExp (Temp.namedLabel "initArray") [size, init]

-- Frame.externalCall "initArray" [size, init]

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

whileExp :: Exp -> Exp -> Temp.Label -> State Temp.TempState Exp
whileExp test body brkdest = do
        let genstm = unCx test
        body' <- unNx body
        l1 <- Temp.newLabel
        l2 <- Temp.newLabel
        return $
                Nx $
                        mkseq
                                [ T.LABEL l1
                                , genstm (l2, brkdest)
                                , T.LABEL l2
                                , body'
                                , T.JUMP (T.NAME l1) [l1]
                                , T.LABEL brkdest
                                ]

breakExp :: Temp.Label -> Exp
breakExp brkdest = Nx $ T.JUMP (T.NAME brkdest) [brkdest]

letExp :: [Exp] -> Exp -> State Temp.TempState Exp
letExp es body = do
        ss <- mapM unNx es
        e <- unEx body
        return $ Ex $ T.ESEQ (mkseq ss) e

staticLink :: T.Exp -> T.Exp
staticLink e = T.MEM (T.BINOP T.PLUS (T.CONST (-3)) e) 0 --tmp 3, 0
