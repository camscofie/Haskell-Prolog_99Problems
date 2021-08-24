primes n = calPrim [2..n]
    where calPrim (p:ps) = p : calPrim (filter (\x -> x `mod` p /= 0) ps)

partialSum (x:xs) = partialAcc xs x
    where partialAcc (y:ys) acc = acc : partialAcc ys (y+acc) 

eins = 1 : -1 : eins

thisFunction = sum (zipWith (/) eins (map (\x -> 2*x + 1) [0..100]))

ins x [] = []
ins x (y:ys)
    | x <= y = x:y:ys
    | otherwise = y : ins x ys

sort [] = []
sort (x:xs) = ins x (sort xs)

foldrX :: (t-> s -> s) ->s -> [t] -> s 
foldrX op i [] = i
foldrX op i (x:xs) = op x (foldrX op i xs )

mapX f [] = []
mapX f (x:xs) = f x : mapX f xs

foldlX op i [] = i
foldlX op i (x:xs) = foldlX op (op i x ) xs

sentenseLength :: [String] -> Int
sentenseLength = foldr (\x n -> lengthX x + n) 0 

lengthX :: [t] -> Int
lengthX = foldr (\x n -> 1 + n) 0

filterX :: (t -> Bool) -> [t] -> [t]
filterX pred [] = []
filterX pred (x:xs)
    | pred x = x : filterX pred xs
    | otherwise = filterX pred xs

hamming l r = sum (zipWith diff l r)
    where diff x y = if (x==y) then 1 else 0

zipWithX :: (s -> t -> u) -> [s] -> [t] -> [u]
zipWithX f (x:xs) (y:ys) = f x y : zipWithX f xs ys
zipWithX f xs ys = []

qsort [] = []
qsort (p:ps) = (qsort [x|x<-ps, x<=p]) ++ p : qsort [x|x<-ps, x > p]

qsort1 [] = []
qsort1 (p:ps) = (qsort (filter (\x -> x <= p) ps)) ++ p : qsort (filter (\x -> x>p) ps)

fak n = fakAcc n 1
    where fakAcc 0 acc = acc
          fakAcc n acc = fakAcc (n-1) n*acc

fib n = fibAcc n 0 1
fibAcc n n1 n2
    | n == 0 = n1
    | n == 1 = n2
    | otherwise = fibAcc (n-1) n2 (n1+n2) 

iter f 0 = (\x -> x)
iter f n = f. iter f (n-1)


main = do
    print $ fib