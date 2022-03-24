module Env where

import qualified Symbol
import qualified Types as T

import qualified Data.Map.Strict as M

data EnvEntry
    = VarEntry {ty :: T.Ty}
    | FunEntry {formals :: [T.Ty], result :: T.Ty}

type BaseTEnv = Symbol.Table T.Ty
type BaseVEnv = Symbol.Table EnvEntry

baseTEnv :: BaseTEnv
baseTEnv = M.fromList [("int", T.INT), ("string", T.STRING)]

baseVEnv :: BaseVEnv
baseVEnv = M.fromList xs
  where
    xs =
        [ ("print", FunEntry [T.STRING] T.UNIT)
        , ("flush", FunEntry [] T.UNIT)
        , ("getchar", FunEntry [] T.STRING)
        , ("ord", FunEntry [T.STRING] T.INT)
        , ("chr", FunEntry [T.INT] T.STRING)
        , ("size", FunEntry [T.STRING] T.INT)
        , ("substring", FunEntry [T.STRING, T.INT, T.INT] T.INT)
        , ("concat", FunEntry [T.STRING, T.STRING] T.STRING)
        , ("not", FunEntry [T.INT] T.INT)
        , ("exit", FunEntry [T.INT] T.UNIT)
        ]