module Compiler.MakeGraph where

import qualified Compiler.Assem as A
import qualified Compiler.Flow as F
import qualified Compiler.Graph as G

import Control.Monad.State

instrs2graph :: [A.Instr] -> (F.FlowGraph, [G.Node])
instrs2graph instrs = undefined

i2g :: A.Instr -> StateT F.FlowGraph (State G.Graph) G.Node
i2g (A.OPER assem src dst jump) = do
        fg <- get
        i <- lift G.newNode
        let def = G.enter i dst `execState` F.def fg
            use = G.enter i src `execState` F.use fg
            ismove = G.enter i (take 4 assem == "MOVE") `execState` F.ismove fg
        g <- lift get
        put fg{F.control = g, F.def = def, F.use = use, F.ismove = ismove}
        return i
i2g _ = undefined
