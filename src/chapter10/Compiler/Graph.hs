module Compiler.Graph where

import qualified Common.Temp as Temp
import Prelude hiding (pred, succ)

import Control.Monad.State
import Data.List (delete)
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

type Node = Int

data Noderep = Node {succ :: [Node], pred :: [Node]} deriving (Eq)

emptyNode :: Noderep
emptyNode = Node{succ = [], pred = []}

bogusNode :: Noderep
bogusNode = Node{succ = [-1], pred = []}

isBogus :: Noderep -> Bool
isBogus Node{succ = (-1 : _)} = True
isBogus _ = False

type Graph = V.Vector Noderep

--nodes :: Graph -> [Node]
--nodes g = f 0
--    where
--        f i =
--                if isBogus (g !! i)
--                        then []
--                        else (g, i) : f (i + 1)

nodes :: Graph -> [Node]
nodes g = f 0
    where
        f i
                | isBogus (g V.! i) = []
                | otherwise = i : f (i + 1)

--succ' :: Node -> [Node]
--succ' (g, i) = map (augment g) s
--    where
--        Node{succ = s} = g !! i

succ' :: Node -> State Graph [Node]
succ' i = do
        g <- get
        let Node{succ = s} = g V.! i
        return s

--pred' :: Node -> [Node]
--pred' (g, i) = map (augment g) p
--    where
--        Node{pred = p} = g !! i

pred' :: Node -> State Graph [Node]
pred' i = do
        g <- get
        let Node{pred = p} = g V.! i
        return p

adj :: Node -> State Graph [Node]
adj gi = (++) <$> pred' gi <*> succ' gi

--eq :: Node -> Node -> Bool
--eq (_, a) (_, b) = a == b

--augment :: Graph -> Node' -> Node
--augment g n = (g, n)

newGraph :: Graph
newGraph = undefined --[bogusNode]

newNode :: State Graph Node
newNode = do
        g <- get
        look g 0 (length g)
    where
        look :: Graph -> Node -> Node -> State Graph Node
        look g lo hi
                | lo == hi = do
                        put (g V.++ newGraph)
                        return lo
                | isBogus (g V.! m) = look g lo m
                | otherwise = look g (m + 1) hi
            where
                m = (lo + hi) `div` 2

update :: Node -> Noderep -> State Graph ()
update i e = undefined

diddleEdge :: (Node -> [Node] -> [Node]) -> Node -> Node -> State Graph ()
diddleEdge change i j = do
        g <- get
        let ni = g V.! i
            nj = g V.! j
        update i ni{succ = change j (succ ni)}
        update j nj{pred = change i (succ nj)}

mkEdge :: Node -> Node -> State Graph ()
mkEdge = diddleEdge (:)

rmEdge :: Node -> Node -> State Graph ()
rmEdge = diddleEdge delete

-- Table
newtype Table a = Table [(Node, a)]

enter :: Node -> a -> State (Table a) ()
enter n v = state $ \(Table list) -> ((), Table ((n, v) : list))
