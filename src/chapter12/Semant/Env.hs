module Semant.Env where

import qualified Common.Symbol as S
import qualified Common.Temp as Temp
import qualified Semant.Translate as TL
import qualified Semant.Types as T

data EnvEntry
        = VarEntry {access :: TL.Access, ty :: T.Ty}
        | FunEntry {level :: TL.Level, label :: Temp.Label, formals :: [T.Ty], result :: T.Ty}

type BaseTEnv = S.Table (TL.Level, T.Ty)
type BaseVEnv = S.Table EnvEntry

baseTEnv :: BaseTEnv
baseTEnv =
        S.new
                [ (S.symbol "int", (TL.Outermost, T.INT))
                , (S.symbol "string", (TL.Outermost, T.STRING))
                , (S.symbol "nil", (TL.Outermost, T.NIL))
                ]

baseVEnv :: BaseVEnv
baseVEnv = S.new $ map funentry reserved
    where
        funentry :: (String, [T.Ty], T.Ty) -> (S.Symbol, EnvEntry)
        funentry (name, fmls, res) = (S.symbol name, FunEntry TL.Outermost (Temp.namedLabel name) fmls res)
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
