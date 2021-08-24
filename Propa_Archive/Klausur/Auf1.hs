pow1 :: Int -> Int -> Int
pow1 b 0 = 1
pow1 b e = b * pow1 b (e-1)


pow2 b e
    | e == 0 = 1
    | e `mod` 2 == 0 = pow2 (b*b) (e `div` 2)
    | otherwise = b * pow2 (b*b) (e `div` 2)


pow3 b e
    | e < 0 = error "it's not right"
    | otherwise = pow3Acc b e 1
    where 
        pow3Acc b e acc
            | e == 0 = acc
            | even e = pow3Acc (b*b) (e `div` 2) acc
            | otherwise = pow3Acc (b*b) (e `div` 2) (acc*b)


root e r 
    | e < 1 = error "not good"
    | r < 0 = error "won't work"
    | otherwise = searchRoot 0 (r+1)
    where 
        searchRoot low high
            | low + 1 == high = low
            | pow3 half e <= r = searchRoot half high
            | otherwise = searchRoot low half
            where half = (low + high) `div` 2 

isPrime n
    | n < 2 = error "not going to work"
    | otherwise = notDivide 2
    where
        notDivide k
            | k > half = True
            | n `mod` k == 0 = False
            | otherwise = notDivide (k+1)
            where half = root 2 n
            

isPrime2 n
    | n < 2 = error "not working"
    | otherwise = null $ filter (\x -> n `mod` x == 0) [3..root 2 n]


insert n [] = [n]
insert n (x:xs)
    | n <= x = n : x : xs
    | otherwise  = x : insert n xs

insertSort [] = []
insertSort (x:xs) = insert x (insertSort xs)

merge :: Ord a => [a] -> [a] -> [a]
merge x [] = x
merge [] y = y
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge
    (mergeSort $ take (length xs `div` 2) xs)
    (mergeSort $ drop (length xs `div` 2) xs)




main = do
    print $ insertSort [3..]
