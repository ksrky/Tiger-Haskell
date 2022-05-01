module Semant.Env where

import qualified Semant.Symbol as S
import qualified Semant.Translate as TL
import qualified Semant.Types as T
import qualified Temp.Temp as Temp

data EnvEntry
        = VarEntry {access :: TL.Access, ty :: T.Ty}
        | FunEntry {level :: TL.Level, label :: Temp.Label, formals :: [T.Ty], result :: T.Ty}

type BaseTEnv = S.Table T.Ty
type BaseVEnv = S.Table EnvEntry

baseTEnv :: BaseTEnv
baseTEnv = S.new [("int", T.INT), ("string", T.STRING), ("nil", T.NIL)]

baseVEnv :: BaseVEnv
baseVEnv = S.new $ map funentry reserved
    where
        funentry :: (S.Symbol, [T.Ty], T.Ty) -> (String, EnvEntry)
        funentry (name, fmls, res) = (name, FunEntry TL.Outermost (Temp.namedLabel name) fmls res)
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
