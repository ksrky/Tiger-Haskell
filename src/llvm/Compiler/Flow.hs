module Compiler.Flow where

import qualified Common.Temp as Temp
import qualified Compiler.Graph as G

import qualified Data.Set as S

data FlowGraph = FGraph
        { control :: G.Graph
        , def :: G.Table (S.Set Temp.Temp)
        , use :: G.Table (S.Set Temp.Temp)
        , ismove :: G.Table Bool
        }

newFlowGraph :: FlowGraph
newFlowGraph = FGraph G.newGraph G.newTable G.newTable G.newTable
