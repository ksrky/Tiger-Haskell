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
nodes = error ""

succ' :: Node -> [Node]
succ' = error ""

pred' :: Node -> [Node]
pred' = error ""

adj :: Node -> [Node]
adj = error ""

eq :: Node -> Node -> Bool
eq (_, a) (_, b) = a == b

augment :: Graph -> Node' -> Node
augment g n = (g, n)

newGraph :: Graph
newGraph = [bogusNode]

newNode :: Graph -> Node
newNode = error ""

mkEdge :: (Node, Node)
mkEdge = undefined

rmEdge :: (Node, Node)
rmEdge = error ""

-- Table
newtype Table a = Table [(Node, a)]

enter :: Table a -> Node -> a -> Table a
enter (Table list) n v = Table ((n, v) : list)
