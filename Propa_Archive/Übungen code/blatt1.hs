module Arithmetik where

pow1 :: Int -> Int -> Int
pow1 b e
  | e < 0 = error "negative exponent"
  | e == 0 = 1
  | otherwise = b * pow1 b (e -1)

pow2 :: Int -> Int -> Int
pow2 b e
  | e < 0 = error "negative exponent"
  | e == 0 = 1
  | e `mod` 2 == 0 = pow2 (b * b) (e `div` 2)
  | otherwise = b * pow2 (b * b) ((e -1) `div` 2)

pow3 :: Int -> Int -> Int
pow3 b e
  | e < 0 = error "negative exponent"
  | otherwise = power3Acc 1 b e
  where
    power3Acc acc b e
      | e == 0 = acc
      | e `mod` 2 == 0 = power3Acc acc (b * b) (e `div` 2)
      | e `mod` 2 == 1 = power3Acc (b * acc) (b * b) (e `div` 2)

searchRoot :: Int -> Int -> Int -> Int -> Int
searchRoot low high e r
  | low + 1 == high = low
  | pow3 half e <= r = searchRoot half high e r
  | otherwise = searchRoot low half e r
  where
    half = (low + high) `div` 2

root :: Int -> Int -> Int
root e r
  | e < 1 = error "negative exponent"
  | r < 0 = error " Negativer Radikant"
  | otherwise = searchRoot 0 (r + 1) e r

-- check if a given int is prime number
isPrime :: Int -> Bool
isPrime x
  | x <= 2 = error "not good bereich"
  | otherwise = findDiv x 2
  where
    upto = root 2 x
    findDiv x acc
      | acc > upto = True
      | x `mod` acc == 0 = False
      | otherwise = findDiv x (acc + 1)

insert :: Int -> [Int] -> [Int]
insert n [] = [n]
insert n (x : xs)
  | n <= x = n : x : xs
  | otherwise = x : insert n xs

-- * this can be improved by reduce the insertSortAcc

insertSort :: [Int] -> [Int]
insertSort [] = []
insertSort (x : xs) = insertSortAcc (x : xs) []
  where
    insertSortAcc [] list = list
    insertSortAcc (x : xs) helpList = insertSortAcc xs (insert x helpList)

-- TODO: use foldr to implement the inserSort function

{- | to test if the insert Sort runs
main = do
  print ("------ below tests insert sort ----- ")
  print (insert 2 [])
  print ( insert 4 [1,3,4,8])
  print (insertSort [3,4,12,2])
  print ( insertSort [5,3,2,92,34,230,3420,320,344,0,(-2),21])
-}

merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge (x : xs) [] = (x : xs)
merge [] (y : ys) = (y : ys)
merge (x : xs) (y : ys)
  | x <= y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

mergeSort :: [Int] -> [Int]
mergeSort [] = []
-- ! mergeSort (x:xs) = merge [x] (mergeSort xs)
-- ! 思路可以，但是这个不是 merge sort 啊！:)

mergeSort [x] = [x]
mergeSort xs = merge (mergeSort (take halfLength xs)) (mergeSort (drop halfLength xs))
  where
    halfLength = (length xs) `div` 2


-- * to test if the mergeSort runs
main = do
  print (merge [2, 3, 4, 12] [5, 10, 29])
  print (mergeSort [2, 4, 21, 25, 12, 211, 250, 13, 45, 21])
  print (mergeSort [2, (-2)])
  print (mergeSort [2, 4, 2, 2, 2, 2, (-2), (-0), 2321, 423, 10204, 21, 23, 1, 23, 295, (-20)])