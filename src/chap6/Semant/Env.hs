module Semant.Env where

import  Symbol
import Semant.Types

data EnvEntry
        = VarEntry Ty
        | FunEntry [Ty] Ty

type BaseTEnv = Table Ty
type BaseVEnv = Table EnvEntry

baseTEnv :: BaseTEnv
baseTEnv = new [("int", INT), ("string", STRING)]

baseVEnv :: BaseVEnv
baseVEnv = new $ map funentry reserved
    where
        funentry :: (Symbol, [Ty], Ty) -> (String, EnvEntry)
        funentry (name, fmls, res) = (name, FunEntry fmls res)
        reserved =
                [ ("print", [STRING], UNIT)
                , ("flush", [], UNIT)
                , ("getchar", [], STRING)
                , ("ord", [STRING], INT)
                , ("chr", [INT], STRING)
                , ("size", [STRING], INT)
                , ("substring", [STRING, INT, INT], INT)
                , ("concat", [STRING, STRING], STRING)
                , ("not", [INT], INT)
                , ("exit", [INT], UNIT)
                ]