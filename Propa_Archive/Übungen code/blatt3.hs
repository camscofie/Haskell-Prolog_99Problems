-- ! 下面的是标准答案，这答案我自己没写出来
fibs :: [Integer]
fibs = 0 : 1 : (zipWith (+) fibs (tail fibs))

-- * calculate next element of Collatz element
collatzNext :: Int -> Int
collatzNext a
  | a `mod` 2 == 0 = a `div` 2
  | otherwise = 3*a + 1

-- * generate an infinite list of Collatz elements 
collatz :: Int -> [Int]
collatz a = iterate collatzNext a

-- list the Collatz list till 2 --> the next element will be 1 
listColl :: Int -> [Int]
listColl m =  (takeWhile (/= 1) (collatz m))

-- * count the Menge of a\n
num :: Int -> Int
num m = length (listColl m)

-- TODO input intervall [a,b] , give back a tuple with biggest num m, m \element [a,b] 
maxNum :: Int -> Int -> (Int, Int)
maxNum a b
  | a < b = error "a is small than b"
  | a < 0 || b < 1 = error "a or b is too small"
  | otherwise = maxPair [a..b]
    where maxPair list = 

maxPair :: [Int] -> (Int, Int)
maxPair [] = []
maxPair (x:xs) = (x,num x) : maxPair xs


main = do
    print (take 20 fibs)
    print (take 30 fibs)

    print (listColl 234)
    print (num 234)
    print (listColl 32)
    print (num 32)
    print (max [23,4,1,21])