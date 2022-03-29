module Env where

import qualified Symbol
import Temp
import qualified Translate as TL
import qualified Types as T

import qualified Data.Map.Strict as M

data EnvEntry
        = VarEntry {access :: TL.Access, ty :: T.Ty}
        | FunEntry {level :: TL.Level, label :: Temp.Label, formals :: [T.Ty], result :: T.Ty}

type BaseTEnv = Symbol.Table T.Ty
type BaseVEnv = Symbol.Table EnvEntry

baseTEnv :: BaseTEnv
baseTEnv = M.fromList [("int", T.INT), ("string", T.STRING)]

baseVEnv :: BaseVEnv
baseVEnv = M.fromList $ map funentry xs
    where
        funentry :: (Symbol.Symbol, [T.Ty], T.Ty) -> (String, EnvEntry)
        funentry (name, formals, result) = (name, FunEntry TL.Outermost (Temp.namedLabel name) formals result)
        xs =
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