module Env where

import qualified Symbol
import qualified Types

data EnvEntry
    = VarEntry {ty :: Types.Ty}
    | FunEntry {formals :: [Types.Ty], result :: Types.Ty}

type BaseTEnv = Symbol.Table Types.Ty
type BaseVEnv = Symbol.Table EnvEntry
