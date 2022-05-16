module Compiler.MakeGraph where

import qualified Compiler.Assem as A
import qualified Compiler.Flow as F
import qualified Compiler.Graph as G

import Control.Monad.State

instrs2graph :: [A.Instr] -> ([G.Node], F.FlowGraph)
instrs2graph instrs = mapM i2g instrs `runState` F.newFlowGraph

i2g :: A.Instr -> State F.FlowGraph G.Node
i2g (A.OPER assem dst src jump) = do
        fg <- get
        let (i, g) = G.newNode `runState` F.control fg
            def = G.enter i dst `execState` F.def fg
            use = G.enter i src `execState` F.use fg
            ismove = G.enter i False `execState` F.ismove fg
        put fg{F.control = g, F.def = def, F.use = use, F.ismove = ismove}
        return i
i2g A.LABEL{} = do
        fg <- get
        let (i, g) = G.newNode `runState` F.control fg
        put fg{F.control = g}
        return i
i2g (A.MOVE assem dst src) = do
        fg <- get
        let (i, g) = G.newNode `runState` F.control fg
            def = G.enter i [dst] `execState` F.def fg
            use = G.enter i [src] `execState` F.use fg
            ismove = G.enter i True `execState` F.ismove fg
        put fg{F.control = g, F.def = def, F.use = use, F.ismove = ismove}
        return i
