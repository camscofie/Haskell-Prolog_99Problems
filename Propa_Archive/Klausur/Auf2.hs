type Polynom = [Double]


cmult :: Polynom -> Double -> Polynom
cmult p d = map (d*) p

eval :: Polynom -> Double -> Double
eval p x = foldr (\i j -> i + j * x ) 0 p

deriv :: Polynom -> Polynom
deriv [] = []
deriv p = zipWith (*) [1..] (tail p)

next :: Int -> Int
next a0
    | a0 `mod` 2 == 0 = a0 `div` 2
    | otherwise = (3 * a0) + 1

collatz :: Int -> [Int]
collatz = iterate next

num :: Int -> Int
num m = length $ takeWhile (/= 1) (collatz m)

maxNum :: Int -> Int -> (Int, Int)
maxNum a b = foldl maxPair (0,0) (makePair a b)
    where maxPair (x,y) (x',y') = if y>y' then (x,y) else (x',y')


makePair :: Int -> Int -> [(Int, Int)]
makePair a b = map (\x -> (x, num x)) [a..b]
      

rot :: String -> String
rot (x:xs) = xs ++ [x]

main = do
    print $ rot "fjkd"
