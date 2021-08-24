data Color = Red | Black
data Tree t = Leaf
            | Node (Tree t) t (Tree t)

instance (Show t) => Show (Tree t) where
    show (Leaf) = "Leaf"
    show (Node left t right) = "Node ( " ++ show left ++" " ++  show t ++" " ++show right++" )"

app :: [t] -> [t] -> [t]
app [] l = l
app (x:xs) right = x : (app xs right) 

someTree :: Tree Integer
someTree = Node (Node (Node Leaf 2 Leaf) 1 Leaf) 3 Leaf

mapT :: (s -> t) -> Tree s -> Tree t
mapT f Leaf = Leaf
mapT f (Node left x right) = Node (mapT f left) (f x) (mapT f right)

foldT :: (s -> t -> s -> s) -> s -> Tree t -> s
foldT f i Leaf = i
foldT f i (Node left t right) = f (foldT f i left) t (foldT f i right)

sizeT :: Tree t -> Integer 
sizeT = foldT (\left x right -> left + 1 + right) 0

heightT :: Tree t -> Integer 
heightT = foldT (\left x right -> 1 + max left right) 0

pathT :: Tree t -> [[t]]
pathT = foldT consAll [[]]
    where consAll leftErgenis t rightErgebnis = map (t:) (leftErgenis ++ rightErgebnis)

search :: Eq s => s -> Tree s -> Bool 
search s Leaf = False 
search s (Node left x right)
    | s == x = True 
    | otherwise = search s left || search s right

main = do
    print $ someTree
    print $ search 1 someTree