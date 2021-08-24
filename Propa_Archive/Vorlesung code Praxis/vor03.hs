
data Shape = Circle Double 
           | Rectangle Double Double 

unitCircle = Circle 1.0
dinA4 = Rectangle 210 297

area :: Shape -> Double
area (Circle a) = a*2
area (Rectangle a b) = a * b

data Tree t = Leaf | Node (Tree t) t (Tree t)

someTree = Node (Node Leaf 1 Leaf) 3 Leaf

size :: Tree t -> Int 
size Leaf = 0
size (Node left x right) = (size left) + 1 + (size right)

height Leaf = 0
height (Node left x right) = 1 + (max (height left)(height right))

mapT :: (s->t) -> Tree s -> Tree t
mapT f Leaf = Leaf
mapT f (Node left x right) = Node (mapT f left) (f x) (mapT f right)

add5 :: Tree Integer -> Tree Integer 
add5 = mapT (+5)

fstT :: Tree (s,t) -> Tree s
fstT = mapT fst

foldT :: (s->t->s->s)->s->Tree t->s
foldT f i Leaf = i
foldT f i (Node left x right) = f (foldT f i left) x (foldT f i right)

sizeT2 = foldT (\left x right -> left + 1 + right) 0
heightT2 = foldT (\left x right -> 1 + max (left right)) 0

paths :: Tree t -> [[t]]
paths tree =  foldT consAll [[]] tree 
    where consAll left x right = mapT (x:) (left ++ right)

data Color = Red | Black
data RedBlackTree t = Leaf | Node Color (RedBlackTree t) t (RedBlackTree t)

fold :: (Color ->s -> t -> s -> s) ->s -> RedBlackTree t -> s
fold f i Leaf = i 
fold f i (Node color left x right) =  f color (fold f i left) x (fold f i right)

mapRB :: (Color -> s -> t) -> RedBlackTree s -> RedBlackTree t
mapRB Leaf = Leaf
mapRB f (Node c left x right) = Node c (mapRB f left) (f c x) (mapRB f right)



main = do
    print $ 