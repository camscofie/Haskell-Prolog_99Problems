-- below is my answer to "Ninety-Nine Haskell Problems"  https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems

{-
Problem 11
(*) Modified run-length encoding.

Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.

Example:

* (encode-modified '(a a a a b c c a a d e e e e))
((4 A) B (2 C) (2 A) D (4 E))
Example in Haskell:

λ> encodeModified "aaaabccaadeeee"
[Multiple 4 'a',Single 'b',Multiple 2 'c',
 Multiple 2 'a',Single 'd',Multiple 4 'e']
-}

--encode :: (Eq a) => [a] -> [(a,)]
encode [] = []
 


main = do
    print $ pack "Haaallooo!!///iiijiji"
    print $ encode "Haaallooo!!/iiijiji"