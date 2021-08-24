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
  | even e = pow2 (b * b) (e `div` 2)
  | otherwise = b * pow2 (b * b) ((e -1) `div` 2)

pow3 :: Int -> Int -> Int
pow3 b e
  | e < 0 = error "negative exponent"
  | otherwise = power3Acc 1 b e
  where
    power3Acc acc b e
      | e == 0 = acc
      | even e = power3Acc acc (b * b) (e `div` 2)
      | odd e = power3Acc (b * acc) (b * b) (e `div` 2)

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