-module(ex).
-author("R.S.").
-compile(export_all).

fact(0) ->
    1;
fact(N) when N>0 ->
   N*fact(N-1).


%% Example: encoding binary trees using atoms and tuples
%% {empty}  for the empty tree
%% {node,Data,LT,RT} for a non-empty tree

t() ->
   {node,12,
        {node,7,{empty},{empty}},
        {node,24,
               {node,18,{empty},{empty}},
	       {empty}}}.

sum({empty}) ->
    0;
sum({node,D,LT,RT}) ->
    D+sum(LT)+sum(RT).

bump({empty}) ->
    {empty};
bump({node,D,LT,RT}) ->
    {node,D+1,bump(LT),bump(RT)}.

bumpl([]) -> 
    [];
bumpl([ H | T ]) -> 
    [ H + 1 | bumpl(T) ].

mirror({empty}) -> 
    {empty};
mirror({node, D, LT, RT}) -> 
    {node, D, mirror(RT), mirror(LT)}.


%% Exercise: implement pre-order traversal
%% For example:
%% ex:pre(ex:t()). 
%% [12, 7, 24, 18]

pre({empty}) -> 
    [];
pre({node, D, LT, RT}) ->
    [D | pre(LT) ++ pre(RT) ].

mapl(_F, []) ->
    [];
mapl(F, [H | T]) -> 
    [ F(H) | mapl(F, T)].

foldr(_F, A, []) -> 
    A;
foldr(F, A, [H | T]) -> 
    F(H, foldr(F, A, T)).