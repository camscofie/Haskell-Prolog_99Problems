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

-- test with main--
main = do
    print $ myButLast [4,5,6]
    print $ myButLast ['a','b','c']