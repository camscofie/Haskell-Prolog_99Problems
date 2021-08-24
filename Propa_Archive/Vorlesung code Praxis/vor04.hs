data MehrBaum t = Leaf | Node [MehrBaum t] t
instance (Show t) => Show (MehrBaum t)where
    show (Leaf) = "Leaf"
    show (Node (x:xs) a) = "Node [" ++ (show x)++ "," ++ (show xs) ++ "]" ++ " " ++ (show a)

someTree = Node [(Node [Leaf] 2), (Node [Node [Leaf] 13, Node [Leaf] 10] 3), (Node [Leaf] 4)] 5

mapT :: (s->t) -> MehrBaum s -> MehrBaum t
mapT f Leaf = Leaf
mapT f (Node (tree:trees) x) = Node ([mapT f tree]++ mapTList f trees) (f x)

mapTList :: (s -> t) -> [MehrBaum s] -> [MehrBaum t]
mapTList f [] = []
mapTList f (y:ys) = [mapT f y] ++ mapTList f ys

add5 :: MehrBaum Integer -> MehrBaum Integer
add5 = mapT (+5)

foldT :: ([a] -> t -> a) -> a -> MehrBaum t -> a
foldT f i Leaf = i
foldT f i (Node (x:xs) n) = f ( [foldT f i x] ++ (foldTList f i xs)) n

foldTList :: ([a] -> t -> a) -> a -> [MehrBaum t] -> [a]
foldTList f i [] = []
foldTList f i (y:ys) = [foldT f i y ] ++ (foldTList f i ys) 


main = do
    print $ someTree
    print $ add5 someTree 