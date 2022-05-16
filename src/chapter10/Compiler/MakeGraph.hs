module Compiler.MakeGraph where

import qualified Compiler.Assem as A
import qualified Compiler.Flow as F
import qualified Compiler.Graph as G

import Control.Monad.State

instrs2graph :: [A.Instr] -> (F.FlowGraph, [G.Node])
instrs2graph = undefined

i2g :: A.Instr -> State F.FlowGraph G.Node
i2g (A.OPER assem src dst jump) = error ""
i2g _ = undefined
