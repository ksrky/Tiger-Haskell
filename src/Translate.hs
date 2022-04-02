module Translate where

import qualified Frame
import qualified Frame.X64Frame as X64Frame
import qualified Temp
import qualified Tree as T

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

newLevel :: Level -> [Bool] -> Temp.State -> (Level, Temp.State)
newLevel par fmls s = (Level par lab fmls frm, s')
    where
        (lab, s') = Temp.app Temp.newLabel s
        frm = Frame.newFrame lab fmls

allocLocal :: Level -> Bool -> Access
allocLocal lev@Level{frame = frm} esc = Access lev acs
    where
        acs = Frame.allocLocal frm esc
allocLocal Outermost _ = undefined

data Exp
        = Ex T.Exp
        | Nx T.Stm
        | Cx ((Temp.Label, Temp.Label) -> T.Stm)

unEx :: Temp.State -> Exp -> (T.Exp, Temp.State)
unEx s (Ex exp) = (exp, s)
unEx s (Nx stm) = (T.ESEQ stm (T.CONST 0), s)
unEx s (Cx genstm) =
        ( T.ESEQ
                ( mkseq
                        [ T.MOVE (T.TEMP r) (T.CONST 1)
                        , genstm (t, f)
                        , T.LABEL f
                        , T.MOVE (T.TEMP r) (T.CONST 0)
                        , T.LABEL t
                        ]
                )
                (T.TEMP r)
        , s'''
        )
    where
        (r, s') = Temp.app Temp.newTemp s
        (t, s'') = Temp.app Temp.newLabel s'
        (f, s''') = Temp.app Temp.newLabel s''

unNx :: Temp.State -> Exp -> (T.Stm, Temp.State)
unNx s (Ex exp) = (T.EXP exp, s)
unNx s (Nx stm) = (stm, s)
unNx s (Cx genstm) =
        ( mkseq
                [ genstm (t, t)
                , T.LABEL t
                ]
        , s'
        )
    where
        (t, s') = Temp.app Temp.newLabel s

unCx :: Exp -> ((Temp.Label, Temp.Label) -> T.Stm)
unCx (Ex exp) = uncurry (T.CJUMP T.NE exp (T.CONST 0))
unCx (Nx _) = undefined
unCx (Cx genstm) = genstm

mkseq :: [T.Stm] -> T.Stm
mkseq [] = T.EXP $ T.CONST 0
mkseq [stm] = stm
mkseq (stm : stms) = T.SEQ stm (mkseq stms)

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
                e' = T.MEM $ T.BINOP T.PLUS (T.CONST (-3)) e --tmp: offset=3

nilExp :: Exp
nilExp = Ex $ T.CONST 0

intExp :: Int -> Exp
intExp i = Ex $ T.CONST i

{-
ifThenElse :: Temp.State -> Exp -> Exp -> Exp -> (Exp, Temp.State)
ifThenElse s test then' else' = (Ex e, temp)
    where
        genstm = unCx test
        (t, s') = unEx s then'
        (f, s'') = unEx s' else'-}