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


