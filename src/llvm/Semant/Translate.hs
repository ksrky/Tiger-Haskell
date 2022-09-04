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

newLevel :: Monad m => Level -> [Bool] -> StateT Temp.TempState m Level
newLevel par fmls = do
        lab <- Temp.newLabel
        frm <- Frame.newFrame lab fmls
        return (Level par lab (True : fmls) frm)

topLevel :: Level
topLevel = Level Outermost lab [True] frm
    where
        lab = Temp.namedLabel "main"
        frm = Frame.newFrame lab [] `evalState` Temp.emptyState

allocLocal :: Monad m => Level -> Bool -> StateT Temp.TempState m Access
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

instance Show Exp where
        show (Ex e) = show e
        show (Nx s) = show s
        show (Cx _) = error "cannot show Cx"

unEx :: Monad m => Exp -> StateT Temp.TempState m T.Exp
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

unNx :: Monad m => Exp -> StateT Temp.TempState m T.Stm
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
simpleVar (Access lev_dec acs, lev_use) = Ex sl
    where
        walkStaticLink :: Level -> T.Exp -> T.Exp
        --walkStaticLink Outermost e = error $ show e
        walkStaticLink lo e
                | name lev_dec == name lo = Frame.exp acs e
                | otherwise = walkStaticLink (parent lo) (calcStaticLink lo e)
        sl = walkStaticLink lev_use (T.TEMP $ Frame.fp $ frame lev_use)

lvalueVar :: Monad m => Exp -> Exp -> StateT Temp.TempState m Exp
lvalueVar var idx = do
        var' <- unEx var
        idx' <- unEx idx
        return $ Ex $ T.BINOP T.PLUS var' (T.BINOP T.MUL idx' Frame.wordSize) -- ? what is wordSize

nilExp :: Exp
nilExp = Ex $ T.CONST 0

intExp :: Int -> Exp
intExp i = Ex $ T.CONST i

stringExp :: Monad m => String -> StateT Temp.TempState m (Exp, Frame.Frag f)
stringExp s = do
        lab <- Temp.newLabel
        return (Ex $ T.NAME lab, Frame.STRING lab s)

callExp :: Monad m => (Level, Level) -> Temp.Label -> [Exp] -> StateT Temp.TempState m Exp
callExp (lev_dec, lev_use) f es = do
        args <- mapM unEx es
        return $ Ex $ T.CALL (T.NAME f) (sl : args)
    where
        walkStaticLink :: Level -> T.Exp -> T.Exp
        walkStaticLink Outermost e = e
        walkStaticLink lo e
                | name lev_dec == name lo = e
                | otherwise = walkStaticLink (parent lo) (calcStaticLink lo e)
        sl = walkStaticLink lev_use (T.TEMP $ Frame.fp $ frame lev_use)

binOp :: Monad m => T.BinOp -> Exp -> Exp -> StateT Temp.TempState m Exp
binOp op left right = do
        left' <- unEx left
        right' <- unEx right
        return $ Ex $ T.BINOP op left' right'

plusOp :: Monad m => Exp -> Exp -> StateT Temp.TempState m Exp
plusOp = binOp T.PLUS

minusOp :: Monad m => Exp -> Exp -> StateT Temp.TempState m Exp
minusOp = binOp T.MINUS

timesOp :: Monad m => Exp -> Exp -> StateT Temp.TempState m Exp
timesOp = binOp T.MUL

divideOp :: Monad m => Exp -> Exp -> StateT Temp.TempState m Exp
divideOp = binOp T.DIV

relOp :: Monad m => T.Relop -> Exp -> Exp -> StateT Temp.TempState m Exp
relOp op left right = do
        left' <- unEx left
        right' <- unEx right
        return $ Cx (uncurry (T.CJUMP op left' right'))

ltOp :: Monad m => Exp -> Exp -> StateT Temp.TempState m Exp
ltOp = relOp T.LT

gtOp :: Monad m => Exp -> Exp -> StateT Temp.TempState m Exp
gtOp = relOp T.GT

leOp :: Monad m => Monad m => Exp -> Exp -> StateT Temp.TempState m Exp
leOp = relOp T.LE

geOp :: Monad m => Exp -> Exp -> StateT Temp.TempState m Exp
geOp = relOp T.GE

eqOp :: Monad m => Exp -> Exp -> StateT Temp.TempState m Exp
eqOp = relOp T.EQ

neqOp :: Monad m => Exp -> Exp -> StateT Temp.TempState m Exp
neqOp = relOp T.NE

recordExp :: Monad m => [Exp] -> StateT Temp.TempState m Exp
recordExp cs = do
        args <- mapM unEx (Ex (T.CONST $ length cs) : cs)
        return $ Ex $ Frame.externalCall "initRecord" args

seqExp :: Monad m => [Exp] -> StateT Temp.TempState m Exp
seqExp [] = return $ Nx $ T.EXP $ T.CONST 0
seqExp exps = do
        e <- unEx (last exps)
        ss <- mapM unNx (init exps)
        return $ case ss of
                [] -> Ex e
                _ -> Ex $ T.ESEQ (mkseq ss) e

assignExp :: Monad m => Exp -> Exp -> StateT Temp.TempState m Exp
assignExp left right = do
        left' <- unEx left
        right' <- unEx right
        return $ Nx $ T.MOVE left' right'

ifExp :: Monad m => Exp -> Exp -> Exp -> StateT Temp.TempState m Exp
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

whileExp :: Monad m => Exp -> Exp -> StateT Temp.TempState m Exp
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

letExp :: Monad m => [Exp] -> Exp -> StateT Temp.TempState m Exp
letExp decs body = do
        ss <- mapM unNx decs
        e <- unEx body
        return $ Ex $ T.ESEQ (mkseq ss) e

arrayExp :: Monad m => Exp -> Exp -> StateT Temp.TempState m Exp
arrayExp size init = do
        args <- mapM unEx [size, init]
        return $ Ex $ Frame.externalCall "initArray" args
