data Tree t = Leaf
            | Node (Tree t ) t (Tree t ) deriving Show

showTree = Node (Node Leaf 2 Leaf) 3 Leaf

foldT :: (s->t->s->s) -> s -> Tree t-> s
foldT f i Leaf = i
foldT f i (Node left t right) = f (foldT f i left) t (foldT f i right) 

mapT :: (s->t) -> Tree s -> Tree t
mapT f Leaf = Leaf
mapT f (Node left t right) = Node (mapT f left) (f t) (mapT f right)

paths :: Tree t -> [[t]]
paths = foldT (\l t r -> map (t:) (l ++ r)) [[]]

ins :: Ord t  => t -> Tree t -> Tree t
ins x Leaf = Node Leaf x Leaf
ins x (Node left v right) 
    | x <= v = let left1 = ins x left in Node left1 v right
    | otherwise = let right1 = ins x right in Node left v right1
    
main = do
    print $ showTree
    print $ paths showTree

