map1 :: (s -> t) -> [s] -> [t]
map1 f [] = []
map1 f (x:xs) = f x : map1 f xs

filter1 :: (t -> Bool) -> [t] -> [t]
filter1 pred1 [] = []
filter1 pred1 (x:xs) = if (pred1 x) then x : filter1 pred1 xs else filter1 pred1 xs

-- Parametrisierte Funktionen: f_a (x) =  a * x^2
f :: Double -> (Double -> Double)
f a = \x -> a * x * x

-- Funktionskomposition: f . g
comp :: (u->t) -> (s->u) -> (s->t)
comp f g = (\x -> f (g x))

-- n-fache Funktionsanwendung: f^n
iter1 :: (t->t) -> Integer -> (t -> t)
iter1 f n
    | n == 0 = (\x -> x)
    | otherwise =  f.(iter1 f (n-1))

sum1 :: [Int] -> Int 
sum1 [] = 0
sum1 (x:xs) = x + sum1 xs

product1 :: [Int] -> Int 
product1 [] = 1
product1 (x:xs) = x * product1 xs

fold1r op init [] = init
fold1r op init (x:xs) = op x (fold1r op init xs)

fold1l op init [] = init
fold1l op init (x:xs) = fold1l op (op init x) xs

-- listenlänge:
length1 :: [t] -> Int 
length1 [] = 0
length1 list = 1 + length (tail list)

length2 :: [t] -> Int 
length2 list = foldr (+) 0 (map (\x -> 1) list)


-- Satzlänge: unterschiedliche Argumenttypen bei lambda-Ausdruck
sentenceLength :: [String] -> Int
sentenceLength = foldr (\l n -> length2 l + n) 0

-- Listenumkehrung
rev :: [t] -> [t]
rev = fold1l cons []
    where cons xs x = x:xs

-- Listenkonkatenation:
app :: [t] -> [t] -> [t]
app l r = foldr (:) r l

-- verflachen von Listen: (vordefiniert als concat)
-- flatten [[1,2], [3], [4,5]] = [1,2,3,4,5]
flatten :: [[t]] -> [t]
flatten = foldr app [] 

-- filtern von Listen
filtern :: (t -> Bool) -> [t] -> [t]
filtern pred2 = foldr check []
    where check x xs = if (pred2 x) then x : xs else xs 

map2 :: (s->t) -> [s] -> [t]
map2 f = foldr (\x xs -> f x : xs) []


zipWith4 f (x:xs) (y:ys) = f x y : zipWith4 f xs ys
zipWith4 f xs ys = []

primes :: Integer -> [Integer]
primes n = sieve [2..n]
    where 
        sieve [] = []
        sieve (p:xs) = p : sieve (filter (not . multipleOf p) xs)
        multipleOf p x = x `mod` p ==0 


-- Summe der ersten 10 Dreierpotenzen
summe3 = foldl (+) 0 (map (3^) [0..2])

-- Summe der ersten 100 Zahlen, die Vielfach von 3 oder 5 sind:
first100 = foldl (+) 0 (take 5 (filter (\x -> x `mod` 3 ==0 || x `mod` 5 ==0) [1..]))


main = do
    print $ map (\x -> x*x) [2,3,4]
    print $ filter1 (<12) [13,24,4,2,13]
    print $ let g = f 42 in g 2
    print $ take 10 (iterate (128/) 2)
    print $ sentenceLength ["abc","ccb"]
    print $ filtern (>8) [2..15]
    print $ map2 (*2) [1..3]
    print $ zipWith4 (+) [1..2] [3..5]
    print $ primes 32
    print $ summe3
    print $ first100