data Tree t = Node t [Tree t] deriving Show

someTree = Node 2 [Node 1 [], Node 3 [Node 1 []] , Node 4 [ Node 1 [], Node 2 []], Node 9 []]

mapT :: (s->t) -> Tree s -> Tree t
mapT f (Node t ts) = Node (f t) (mapTs f ts)
    where mapTs f list = map (mapT f) list 

foldT :: ( t -> [s] -> s) -> s -> Tree t -> s
foldT f i (Node t ts) = f t (foldTs f i ts)
    where foldTs f i [] = [i]
          foldTs f i (x:xs) = foldT f i x : foldTs f i xs

sizeT :: Tree t -> Int 
sizeT = foldT (\t ts -> 1 + sum ts) 0 

heightT :: Tree t -> Int
heightT = foldT (\t ts -> 1 + maxOf ts) 0  
    where maxOf [x] = x
          maxOf (x:y:xs) = maxOf (max x y : xs) 

searchT :: Eq t => t -> Tree t -> Bool 
searchT key (Node t ts)
    | key == t = True 
    | otherwise = searchTs key ts
        where searchTs key [] = False 
              searchTs key (x:xs) = searchT key x || searchTs key xs

isIn (Node t li) x
    | t == x = True 
    | otherwise = isIn1 li [] x

isIn1 [] [] x = False
isIn1 [] next x = isIn1 next [] x
isIn1 (Node t ts:rs) next x
    | (t==x) = True
    | otherwise = isIn1 rs (ts++next) x

rotateT :: Tree t -> Tree t
rotateT (Node t []) = Node t []
rotateT (Node t ts) = Node t (rotateTs ts)
    where rotateTs [] = []
          rotateTs (x:xs) = rotateTs xs ++ [rotateT x]

paths :: Tree t -> [[t]]
paths = foldT (\x li -> map (x:) (concat li)) [[]]

main = do
    print $ someTree
    print $ mapT (+2) someTree
    print $ sizeT someTree
    print $ heightT someTree
    print $ searchT 3 someTree
    print $ rotateT someTree
    print $ paths someTree
    print $ concat ["hallo","world","are you okay"]
 
 
--    print $ rotateT someTree
--    print $ sizeT someTree
--    print $ heightT someTree
--    print $ searchT 2 someTree