module Sort where

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

merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge (x : xs) [] = x : xs
merge [] (y : ys) = y : ys
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
    halfLength = length xs `div` 2

