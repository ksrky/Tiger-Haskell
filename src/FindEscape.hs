module FindEscape where

import Absyn
import Symbol

type Depth = Int
type EscEnv = Symbol.Table (Depth, Bool)

traverseVar :: (EscEnv, Depth, Absyn.Var) -> ()
traverseVar = undefined

traverseExp :: (EscEnv, Depth, Absyn.Exp) -> ()
traverseExp = undefined

traverseDecs :: (EscEnv, Depth, [Absyn.Dec]) -> ()
traverseDecs = undefined

findEscape :: Absyn.Exp -> ()
findEscape = undefined