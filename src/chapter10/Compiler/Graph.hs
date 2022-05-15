module Compiler.Graph where

import qualified Common.Temp as Temp
import Prelude hiding (pred, succ)

import qualified Data.Map.Strict as M

type Node' = Int

data Noderep = Node {succ :: [Node'], pred :: [Node']}

emptyNode :: Noderep
emptyNode = Node{succ = [], pred = []}

bogusNode :: Noderep
bogusNode = Node{succ = [-1], pred = []}

isBogus :: Noderep -> Bool
isBogus Node{succ = (-1 : _)} = True
isBogus _ = False

type Graph = [Noderep]

type Node = (Graph, Node')

nodes :: Graph -> [Node]
nodes g = f 0
    where
        f i =
                if isBogus (g !! i)
                        then []
                        else (g, i) : f (i + 1)

succ' :: Node -> [Node]
succ' (g, i) = map (augment g) s
    where
        Node{succ = s} = g !! i

pred' :: Node -> [Node]
pred' (g, i) = map (augment g) p
    where
        Node{pred = p} = g !! i

adj :: Node -> [Node]
adj gi = pred' gi ++ succ' gi

eq :: Node -> Node -> Bool
eq (_, a) (_, b) = a == b

augment :: Graph -> Node' -> Node
augment g n = (g, n)

newGraph :: Graph
newGraph = [bogusNode]

newNode :: Graph -> Node
newNode g = look 0 (1 + 3)
    where
        look lo hi
                | lo == hi = undefined
                | isBogus (g !! m) = look lo m
                | otherwise = look (m + 1) hi
            where
                m = (lo + hi) `div` 2

{-}
check g g' = error ""

delete (i, j : rest) = if i == j then rest else j : delete (i, rest)
delete (_, []) = error ""

didleEdge change (g : graph, i) (g' : graph', j) = do
        check g g'
        let Node{succ=si, pred=pi}=g!!i
            Node{succ=sj, pred=pj}= g!!j
        return ()

mkEdge :: (Node, Node)
mkEdge = undefined

rmEdge :: (Node, Node)
rmEdge = error ""
-}
-- Table
newtype Table a = Table [(Node, a)]

enter :: Table a -> Node -> a -> Table a
enter (Table list) n v = Table ((n, v) : list)
