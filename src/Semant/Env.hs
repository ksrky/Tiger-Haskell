module Semant.Env where

import qualified Semant.Symbol as S
import qualified Semant.Types as T

data EnvEntry
        = VarEntry {ty :: T.Ty}
        | FunEntry {formals :: [T.Ty], result :: T.Ty}

type BaseTEnv = S.Table T.Ty
type BaseVEnv = S.Table EnvEntry

baseTEnv :: BaseTEnv
baseTEnv = S.new [("int", T.TCon T.INT), ("string", T.TCon T.STRING), ("nil", T.TCon T.NIL)]

baseVEnv :: BaseVEnv
baseVEnv = S.new $ map funentry reserved
    where
        funentry :: (S.Symbol, [T.TyCon], T.TyCon) -> (String, EnvEntry)
        funentry (name, fmls, res) = (name, FunEntry (map T.TCon fmls) (T.TCon res))
        reserved =
                [ ("print", [T.STRING], T.UNIT)
                , ("flush", [], T.UNIT)
                , ("getchar", [], T.STRING)
                , ("ord", [T.STRING], T.INT)
                , ("chr", [T.INT], T.STRING)
                , ("size", [T.STRING], T.INT)
                , ("substring", [T.STRING, T.INT, T.INT], T.INT)
                , ("concat", [T.STRING, T.STRING], T.STRING)
                , ("not", [T.INT], T.INT)
                , ("exit", [T.INT], T.UNIT)
                ]
