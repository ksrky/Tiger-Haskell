module Semant.Translate where

import qualified Core.Tree as T
import qualified Frame.Frame as Frame
import qualified Frame.X64Frame as X64Frame
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
        lab <- state Temp.newLabel
        frm <- Frame.newFrame lab fmls
        return (Level par lab (True : fmls) frm) -- ?

allocLocal :: Level -> Bool -> State Temp.TempState Access
allocLocal lev@Level{frame = frm} esc = do
        frm' <- Frame.allocLocal esc frm
        return $ Access lev{frame = frm'} (last $ Frame.locals frm')
allocLocal Outermost _ = undefined

data Exp
        = Ex T.Exp
        | Nx T.Stm
        | Cx ((Temp.Label, Temp.Label) -> T.Stm)

unEx :: Exp -> State Temp.TempState T.Exp
unEx (Ex exp) = return exp
unEx (Nx stm) = return $ T.ESEQ stm (T.CONST 0)
unEx (Cx genstm) = do
        r <- state Temp.newTemp
        t <- state Temp.newLabel
        f <- state Temp.newLabel
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
        t <- state Temp.newLabel
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

{-
simpleVar :: (Access, Level) -> Exp
simpleVar (Access lev_dec acc, lev_use) = Ex $ Frame.exp acc t_exp
    where
        t_exp = followStaticLink lev_dec lev_use (T.TEMP $ Frame.fp $ frame lev_use)
        followStaticLink :: Level -> Level -> T.Exp -> T.Exp
        followStaticLink hi lo e =
                if hi == lo
                        then e
                        else followStaticLink hi (parent lo) e'
            where
                e' = Frame.exp (-3) e -}

nilExp :: Exp
nilExp = Ex $ T.CONST 0

intExp :: Int -> Exp
intExp i = Ex $ T.CONST i