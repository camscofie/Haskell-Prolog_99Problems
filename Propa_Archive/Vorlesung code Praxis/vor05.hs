data Tree t = Leaf | Node (Tree t) t (Tree t)
instance (Show t) => Show (Tree t) where
    show (Leaf) = "Leaf"
    show (Node left a right) = "Node [" ++ (show left )++ "," ++ (show right) ++ "]" ++ " " ++ (show a)

someTree = Node (Node Leaf 1 Leaf) 3 (Node Leaf 21 Leaf)

mapT :: (s -> t) -> Tree s -> Tree t
mapT f Leaf = Leaf
mapT f (Node left x right) = Node (mapT f left) (f x) (mapT f right)  

add5 = mapT (+5)

foldT :: (t1 -> t2 -> t1 -> t1) -> t1 -> Tree t2 -> t1
foldT f i Leaf = i
foldT f i (Node left x right) = f (foldT f i left) x (foldT f i right)

sizeT = foldT (\left x right -> left + 1 + right) 0
heightT = foldT (\left x right -> (max left right) + 1) 0

inTree tree x = if (inTree1 tree x == 0) then False else True 
inTree1 Leaf _ = 0
inTree1 (Node left a right) x
    | a == x = 1
    | otherwise = max (inTree1 left x) (inTree1 right x)



main = do
    print $ someTree
    print $ inTree someTree 1
    print $ inTree someTree 1929
    print $ add5 someTree
    print $ sizeT someTree
    print $ heightT someTree