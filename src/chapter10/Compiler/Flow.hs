module Compiler.Flow where

import qualified Common.Temp as Temp
import qualified Compiler.Graph as G

import Control.Monad.State

data FlowGraph = FGraph
        { control :: G.Graph
        , def :: G.Table [Temp.Temp]
        , use :: G.Table [Temp.Temp]
        , ismove :: G.Table Bool
        }
