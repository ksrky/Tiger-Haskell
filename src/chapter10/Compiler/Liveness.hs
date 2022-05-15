module Compiler.Liveness where

import qualified Common.Temp as Temp
import qualified Compiler.Flow as Flow
import qualified Compiler.Graph as Graph

data IGraph = IGraph
        { graph :: Graph.Graph
        , tnode :: Temp.Temp -> Graph.Node
        , gtemp :: Graph.Node -> Temp.Temp
        , moves :: [(Graph.Node, Graph.Node)]
        }

interferenceGraph :: Flow.FlowGraph -> (IGraph, Graph.Node -> [Temp.Temp])
interferenceGraph = undefined
