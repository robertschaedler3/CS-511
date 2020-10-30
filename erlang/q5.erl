-module(q5).

-compile(export_all).

%% Empty tree: {empty}
%% Non-Empty tree: {node,Data,LT,RT}

-type btree() :: {empty} | {node, number(), btree(), btree()}.

-spec t1() -> btree().
t1() ->
    {node,
     12,
     {node, 7, {empty}, {empty}},
     {node, 24, {node, 18, {empty}, {empty}}, {empty}}}.

-spec t2() -> btree().
t2() ->
    {empty}.

-spec t3() -> btree().
t3() ->
    {node, 18, {empty}, {empty}}.

-spec t4() -> btree().
t4() ->
    {node,
     12,
     {node, 7, {node, 3, {empty}, {empty}}, {node, 12, {empty}, {empty}}},
     {node, 24, {node, 18, {empty}, {empty}}, {empty}}}.

%%% Examples:
%% 1> c(q5).
%% {ok,q5}
%% 2> q5:paths_to_leaves(q5:t1()).
%% [[0],[1,0]]
%% 3> q5:paths_to_leaves(q5:t2()).
%% []
%% 4> q5:paths_to_leaves(q5:t3()).
%% [[]]
%% 5> q5:paths_to_leaves(q5:t4()).
%% [[0,0],[0,1],[1,0]]

mapl(_F, []) ->
    [];
mapl(F, [H | T]) -> 
    [ F(H) | mapl(F, T)].

paths_to_leaves({empty}) ->
    [];
paths_to_leaves({node, _, {empty}, {empty}}) ->
    [[]];
paths_to_leaves({node, _, LT, RT}) ->
    mapl(fun (P) -> [0 | P] end, paths_to_leaves(LT))
    ++
    mapl(fun (P) -> [1 | P] end, paths_to_leaves(RT)).
