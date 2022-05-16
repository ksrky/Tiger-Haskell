module Compiler.Liveness where

import qualified Common.Temp as Temp
import qualified Compiler.Flow as F
import qualified Compiler.Graph as G

data IGraph = IGraph
        { graph :: G.Graph
        , tnode :: Temp.Temp -> G.Node
        , gtemp :: G.Node -> Temp.Temp
        , moves :: [(G.Node, G.Node)]
        }

interferenceGraph :: F.FlowGraph -> (IGraph, G.Node -> [Temp.Temp])
interferenceGraph (F.FGraph g def use ismove) = undefined
