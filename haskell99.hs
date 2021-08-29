-- below is my answer to "Ninety-Nine Haskell Problems"  https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems


{- Problem 1 

Find the last element of a list.

λ> myLast [1,2,3,4]
4
λ> myLast ['x','y','z']
'z' -}

import Data.Array.Base (listArrayST)
myLast :: [a] -> a
myLast [] = error "nothing in the list"
myLast [a] = a
myLast (_:xs) = myLast xs


{- Problem 2

 Find the last but one element of a list.

(Note that the Lisp transcription of this problem is incorrect.)

Example in Haskell:

λ> myButLast [1,2,3,4]
3
λ> myButLast ['a'..'z']
'y' -}

myButLast :: [a] -> a
myButLast [] = error "this thing doesn't work"
myButLast [a] = error "also not working"
myButLast [a,_] = a
myButLast (_:xs) = myButLast xs

{-
    Problem 3
(*) Find the K'th element of a list. The first element in the list is number 1.

Example:
    * (element-at '(a b c d e) 3)
    c

Example in Haskell:
    λ> elementAt [1,2,3] 2
    2
    λ> elementAt "haskell" 5
    'e'
-}

elementAt :: (Ord t, Num t) => [p] -> t -> p
elementAt [] _ = error "Not good"
elementAt (x:xs) n
    | n < 0 = error "Not good"
    | n == 0 = x 
    | otherwise = elementAt xs (n-1)

{-
Problem 4
(*) Find the number of elements of a list.

Example in Haskell:

λ> myLength [123, 456, 789]
3
λ> myLength "Hello, world!"
13
-}

myLength :: [a] -> Integer
myLength [] = 0
myLength list = foldr (\x y-> 1+y) 0 list-- test with main--


{-
Problem 5
(*) Reverse a list.

Example in Haskell:

λ> myReverse "A man, a plan, a canal, panama!"
"!amanap ,lanac a ,nalp a ,nam A"
λ> myReverse [1,2,3,4]
[4,3,2,1]
-}

myAppend :: [a] -> [a] -> [a]
myAppend [] list = list
myAppend (x:xs) list = x : (myAppend xs list) 

myReverse :: [a] -> [a]
myReverse list = myReverseAcc list []

myReverseAcc :: [a] -> [a] -> [a]
myReverseAcc [] acc = acc
myReverseAcc (x:xs) acc = myReverseAcc xs (myAppend [x] acc)

{-
Problem 6
(*) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).

Example in Haskell:

λ> isPalindrome [1,2,3]
False
λ> isPalindrome "madamimadam"
True
λ> isPalindrome [1,2,4,8,16,8,4,2,1]
True
-}

isPalindrome ::Ord a => [a] -> Bool
isPalindrome list = isPalindromeAcc list revList
    where
    revList = myReverse list

isPalindromeAcc :: Ord a=>[a]->[a]->Bool
isPalindromeAcc [] [] = True
isPalindromeAcc (x:xs) (y:ys)
    | x == y = isPalindromeAcc xs ys
    | otherwise = False


main = do
    print $ isPalindrome [23,34,5,6]
    print $ isPalindrome [23,3,23]