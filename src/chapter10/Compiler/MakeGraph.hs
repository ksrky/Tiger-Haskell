module Compiler.MakeGraph where

import qualified Common.Temp as Temp
import qualified Compiler.Assem as A
import qualified Compiler.Flow as F
import qualified Compiler.Graph as G

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Maybe

instrs2graph :: [A.Instr] -> State F.FlowGraph [G.Node]
instrs2graph instrs = do
        (nodes, table) <- runWriterT $ mapM createNode instrs
        mapM createEdges (zip instrs nodes) `runReaderT` table

createNode :: A.Instr -> WriterT [(Temp.Label, G.Node)] (State F.FlowGraph) G.Node
createNode (A.OPER assem dst src jump) = do
        fg <- get
        let (i, g) = G.newNode `runState` F.control fg
            def = G.enter i dst `execState` F.def fg
            use = G.enter i src `execState` F.use fg
            ismove = G.enter i False `execState` F.ismove fg
        -- convGraph $ Data.Maybe.fromMaybe [] jump
        put fg{F.control = g, F.def = def, F.use = use, F.ismove = ismove}
        return i
createNode (A.LABEL assem lab) = do
        fg <- get
        let (i, g) = G.newNode `runState` F.control fg
        put fg{F.control = g}
        tell [(lab, i)]
        return i
createNode (A.MOVE assem dst src) = do
        fg <- get
        let (i, g) = G.newNode `runState` F.control fg
            def = G.enter i [dst] `execState` F.def fg
            use = G.enter i [src] `execState` F.use fg
            ismove = G.enter i True `execState` F.ismove fg
        put fg{F.control = g, F.def = def, F.use = use, F.ismove = ismove}
        return i

createEdges :: (A.Instr, G.Node) -> ReaderT [(Temp.Label, G.Node)] (State F.FlowGraph) G.Node
createEdges (A.OPER assem dst src jump, i) = do
        fg <- get
        table <- ask
        let labs = Data.Maybe.fromMaybe [] jump
            Just succ = forM labs $ \l -> lookup l table
        i' <- lift $ convGraph $ G.next i
        lift $ convGraph $ mapM_ (G.mkEdge i) (i' ++ succ)
        return i
createEdges (_, i) = do
        i' <- lift $ convGraph $ G.next i
        lift $ convGraph $ mapM_ (G.mkEdge i) i'
        return i

convGraph :: Monad m => State G.Graph a -> StateT F.FlowGraph m a
convGraph s = do
        fg <- get
        g <- gets F.control
        let (val, g') = runState s g
        put fg{F.control = g'}
        return val
