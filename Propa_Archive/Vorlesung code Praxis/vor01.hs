qsort [] = []
qsort (p:ps) = (qsort [x| x<-ps, x<=p]) ++ p : (qsort [x|x<-ps, x >p])

qsort2 [] = []
qsort2 (p:ps) = (qsort2 (filter(<=p) ps)) ++ p:(qsort2 (filter(>p) ps))

qsort3 [] = []
qsort3 (p:ps) = (qsort3 (filter (\x -> x<=p) ps)) ++ p:(qsort3(filter(\x -> x>p) ps))

qsort4 :: Ord a => [a] -> [a]
qsort4 [] = []
qsort4 (p:ps) = (qsort4 (filter (<=p) ps)) ++ p:(qsort4(filter (>p) ps))

binom n k =
    if (k==0) || (k ==n)
    then 1
    else binom (n-1) (k-1) + binom (n-1) k

fib n
    | (n == 0 ) = 0
    | (n==1) = 1
    | otherwise = fib (n-1) + fib (n-2)

fibAcc n n1 n2
    | n == 0 = n1
    | n == 1 = n2
    | otherwise = fibAcc (n-1) n2 (n2+ n1)

fib2 :: (Eq t1, Num t1, Num t2) => t1 -> t2
fib2 n = fibAcc n 0 1

length1 list = if (null list) then 0 else 1 + length1 (tail list)

isIn x (y:xs)
    | null (y:xs) = False 
    | x == y = True 
    | otherwise = isIn x xs

isIn2 x list = 
    if (null list)
         then False 
    else 
        if (x == head list) then True else isIn2 x (tail list)

isIn3 x [] = False 
isIn3 x (y:ys) = if (x==y) then True else isIn3 x ys

maximum1 list = if (null list) then error "empty" 
                else 
                    if (null (tail list)) then (head list) 
                    else max (maximum1 (tail list)) (head list)

maximum2 [] = error "empty"
maximum2 (x:[]) = x
maximum2 (x:xs) = max x (maximum xs)

app [] right = right
app (x:xs) right = x : (app xs right)

app2 [] right = right
app2 left right = foldr (:) right left

rev [] = []
rev (x:xs) = app2 (rev xs) [x]



main = do
    print $ binom 3 2
    print $ qsort [25,32,43,93,2,34]
    print $ qsort2 [25,32,43,93,2,34]
    print $ qsort3 [25,32,43,93,2,34]
    print $ qsort4 [25,32,43,93,2,34]
    print $ map (/2) [1..4]
    print $ [x*x| x <- [1..10], x <= 5]
    print $ fib2 10
    print $ length1 [1..21]
    print $ isIn2 22 [2..20]
    print $ maximum2 [3,4,53,34,31,34,39]
    print $ rev [1..10]
     