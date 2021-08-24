data Node t = Node t [Node t] deriving Show
data Graph t = Graph [Node t] deriving Show


nodeA :: Node [Char]
nodeA = Node "A" [Node "B" [], Node "C" []]
nodeB :: Node [Char]
nodeB = Node "B" [Node "A" []]
nodeC :: Node [Char]
nodeC = Node "C" [Node "A" []]

someGraph :: Graph [Char]
someGraph = Graph [nodeA, nodeB, nodeC]


mapN :: (s->t) -> Node s -> Node t
mapN f (Node t nodes) = Node (f t) (mapNs f nodes)

mapNs :: (s -> t) -> [Node s] -> [Node t]
mapNs f = map (mapN f)

mapG :: (s->t) -> Graph s -> Graph t
mapG f (Graph nodes) = Graph (mapNs f nodes) 


--search :: Eq t =>  t -> Graph t -> Bool
--search key (Graph []) = False 
--search key (Graph nodes) = somethingTrue (map (searchNode key) nodes)
--    where searchNode key (Node t ts) = key == t 
--          somethingTrue [] = False
--          somethingTrue (x:xs) = x || somethingTrue xs

main = do
    print $ someGraph
    print $ mapG (++"abc") someGraph
    --print $ search "A" someGraph

