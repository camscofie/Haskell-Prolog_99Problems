-- below is my answer to "Ninety-Nine Haskell Problems"  https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems


{- Problem 1 

Find the last element of a list.

λ> myLast [1,2,3,4]
4
λ> myLast ['x','y','z']
'z' -}

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


-- test with main--
main = do
    print $ elementAt [3,4,5] 3