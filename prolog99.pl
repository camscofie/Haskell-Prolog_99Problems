/ below is my answer to "Prolog Problems"  https://sites.google.com/site/prologsite/prolog-problems

/*
1.01 (*) Find the last element of a list.
Example:
?- my_last(X,[a,b,c,d]).
X = d */

my_last(X,[X]):-!.
my_last(X,[_|B]) :- my_last(X,B).

/* 
1.02 (*) Find the last but one element of a list.
(de: zweitletztes Element, fr: avant-dernier élément)
*/

my_lastButOne(X,[X,_]):-!.
my_lastButOne(X,[_|B]):- my_lastButOne(X,B).

/*
1.03 (*) Find the K'th element of a list.
The first element in the list is number 1.
Example:
?- element_at(X,[a,b,c,d,e],3).
X = c
/*

element_at(X,[X|_],1):-!.
element_at(A,[_|L],M):-M>0,N is M-1,element_at(A,L,N). 

/*
1.04 (*) Find the number of elements of a list.
*/


count(0,[]).
count(A, [_|L]):- count(A1,L),A is A1+1.


/*
1.05 (*) Reverse a list.
*/

myAppend([],L,L).
myAppend([A|B],L,[A|X]):-myAppend(B,L,X).

myReverse([],[]).
myReverse([A|B],R):-myReverse(B,RB),myAppend(RB,[A],R).

/*
1.06 (*) Find out whether a list is a palindrome.
A palindrome can be read forward or backward; e.g. [x,a,m,a,x].
*/

isPalindrome([]).
isPalindrome(X):-myReverse(X,Y),isSame(X,Y).

isSame([],[]).
isSame([X|XS],[X|YS]):-isSame(XS,YS).
             
             
myAppend([],L,L).
myAppend([A|B],L,[A|X]):-myAppend(B,L,X).

myReverse([],[]).
myReverse([A|B],R):-myReverse(B,RB),myAppend(RB,[A],R).


/*
1.07 (**) Flatten a nested list structure.
Transform a list, possibly holding lists as elements into a 'flat' list by replacing each list with its elements (recursively).

Example:
?- my_flatten([a, [b, [c, d], e]], X).
X = [a, b, c, d, e]
*/

my_flatten([],[]).
my_flatten([A|B],Final):-isListy(A),my_flatten(A,AF),my_flatten(B,BF),myAppend(AF,BF,Final).
my_flatten([A|B],Final):- \+ isListy(A),my_flatten(B,BF),myAppend([A],BF,Final).

isListy([]).
isListy([_|_]).

myAppend([],L,L).
myAppend([A|B],L,[A|X]):-myAppend(B,L,X).

/*
1.08 (**) Eliminate consecutive duplicates of list elements.
If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.

Example:
?- compress([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
X = [a,b,c,a,d,e]
*/

compress([X],[X]).
compress([X,Y|XS],Com):- X=Y, compress([Y|XS],Com).
compress([X,Y|XS],Com):- not(X=Y),compress([Y|XS],YS),myAppend([X],YS,Com).

myAppend([],L,L).
myAppend([A|B],L,[A|X]):-myAppend(B,L,X).