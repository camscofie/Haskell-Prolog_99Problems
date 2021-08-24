data Tree t = Node t [Tree t] deriving Show


someTree :: Tree Integer
someTree = Node 32 [Node 2 [], Node 3 [Node 5 []], Node 4 []]

mapT :: (t->s) -> Tree t -> Tree s
mapT f (Node t []) = Node (f t) []
mapT f (Node t trees) = Node (f t) (mapForrest f trees)
    where mapForrest :: (t->s)-> [Tree t] -> [Tree s]
          mapForrest f [] = []
          mapForrest f (x:xs) = mapT f x : mapForrest f xs

foldT ::(t -> [s] -> s) -> s -> Tree t -> s
foldT f i (Node x forrest) = f x (foldForrest f i forrest)
    where foldForrest :: (t->[s]->s) -> s -> [Tree t] -> [s]
          foldForrest f i [] = [i]
          foldForrest f i (tree:forrest) = foldT f i tree : foldForrest f i forrest


sizeT :: Tree t -> Int
sizeT = foldT (\t forrest -> 1 + sumT forrest) 0
    where sumT :: [Int] -> Int 
          sumT [] = 0
          sumT (x:xs) =x + (sumT xs)

heightT :: Tree t -> Int
heightT = foldT (\t ts -> maxT ts + 1) 0
    where maxT [] = 0
          maxT (x:xs) = if x > (maxT xs) then x else (maxT xs)

traversalT :: Tree t -> [[t]]
traversalT = foldT (\t ts -> map (t:) (conli ts)) [[]]
    where conli :: [[t]] -> [t]
          conli = foldr (app) []

app :: [t] ->[t] ->[t]
app [] right = right
app (x:xs) right = x : app (xs) right

reverseT :: Tree t -> Tree t
reverseT (Node x []) = Node x []
reverseT (Node x forrest) = Node x (reverseTs forrest )
    where reverseTs [] = []
          reverseTs (tree:trees) = app (reverseTs trees) [reverseT tree]

type KeyPair = [(Integer,String)]

searchT :: key -> Tree KeyPair -> Tree KeyPair 

main = do
    print $ someTree
    print $ mapT (+2) someTree
    print $ sizeT someTree
    print $ heightT someTree
    print $ traversalT someTree
    print $ reverseT someTree