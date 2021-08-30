-- below is my answer to "Ninety-Nine Haskell Problems"  https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems


{- Problem 1 

Find the last element of a list.

λ> myLast [1,2,3,4]
4
λ> myLast ['x','y','z']
'z' -}

import Data.Array.Base (listArrayST)
import GHC.Classes (Eq)
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

{-
Problem 7
(**) Flatten a nested list structure.

Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).

Example:

* (my-flatten '(a (b (c d) e)))
(A B C D E)
Example in Haskell:

We have to define a new data type, because lists in Haskell are homogeneous.

 data NestedList a = Elem a | List [NestedList a]
λ> flatten (Elem 5)
[5]
λ> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
[1,2,3,4,5]
λ> flatten (List [])
[]
-}

data NestedList a = OneElem a | OneList [NestedList a] deriving (Show)
flatten :: NestedList a -> [a]
flatten (OneElem a) = [a]
flatten (OneList []) = []
flatten (OneList (x:xs)) = myAppend (flatten x) (flatten (OneList xs))

{-
Problem 8
(**) Eliminate consecutive duplicates of list elements.

If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.

Example:

* (compress '(a a a a b c c a a d e e e e))
(A B C A D E)
Example in Haskell:

λ> compress "aaaabccaadeeee"
"abcade"
-}

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [a] = [a]
compress (x:y:xs)
    | x == y = compress (x:xs)
    | otherwise = myAppend [x] (compress (y:xs))
 

main = do
    print $ compress "aaaabccaadeeee"