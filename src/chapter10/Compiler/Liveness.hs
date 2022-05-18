module Compiler.Liveness where

import qualified Common.Temp as Temp
import qualified Compiler.Flow as F
import qualified Compiler.Graph as G

import Control.Monad.State
import qualified Data.Set as S

data IGraph = IGraph
        { graph :: G.Graph
        , tnode :: Temp.Temp -> G.Node
        , gtemp :: G.Node -> Temp.Temp
        , moves :: [(G.Node, G.Node)]
        }

type LiveSet = [Temp.Temp]
type LiveMap = G.Table LiveSet

data Liveness = Liveness {liveIn :: G.Table (S.Set Temp.Temp), liveOut :: G.Table (S.Set Temp.Temp)}

emptyLiveness :: Liveness
emptyLiveness = Liveness{liveIn = G.newTable, liveOut = G.newTable}

interferenceGraph :: F.FlowGraph -> (IGraph, G.Node -> [Temp.Temp])
interferenceGraph (F.FGraph g def use ismove) = undefined
    where
        nodes = G.nodes g
        repeat :: State Liveness ()
        repeat = do
                live <- get
                in' <- gets liveIn
                out <- gets liveOut
                forM_ nodes $ \i -> do
                        let in'' = (use G.!? i) `S.union` ((out G.!? i) `S.difference` (def G.!? i))
                            out' = S.unions (map (in' G.!?) (G.succ $ g `G.at` i))
                        put $ live{liveIn = G.enter in' i in'', liveOut = G.enter out i out'}
                in'' <- gets liveIn
                out' <- gets liveOut
                unless ((in' == in'') && (out == out')) repeat
