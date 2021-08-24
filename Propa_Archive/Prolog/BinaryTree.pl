member(X,[X|_]) :- ! .
member(X,[_|Rest]) :- member(X,Rest).

% use member to find a value in Prolog isn't efficient, as it has to iterate through the whole list.
% There I'd like to give a shot for binary search tree

% Rule 1: atom "nil" is Leaf
% Rule 2: a tree has two Node and its value

% Example

tree(nil,15,nil).              % a tree with some value
tree(tree(nil,3,nil),4,nil).  % this is a search tree

%%%%%%%%%%%%%%%%%%%%%

% Search function to dfs in left most branch

search(X,tree(_,X,_)):-!.
search(X,tree(L,_,_)):-search(X,L).
search(X,tree(_,_,R)):-search(X,R).

% Search function in a search tree

search1(X,tree(_,X,_)):-!.
search1(X,tree(L,Value,_)):- X =< Value, search1(X,L).
search1(X,tree(_,Value,R)):- X >= Value, search1(X,R).
