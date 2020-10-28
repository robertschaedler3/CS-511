-module(q5).
-compile(export_all).

-type btree() :: {empty}
	      |  {node,number(),btree(),btree()}.

-spec t1() -> btree().
t1() ->
    {node,1,{node,2,{empty},{empty}},{node,3,{empty},{empty}}}.

-spec t2() -> btree().
t2() ->
    {node,1,
     {node,2,{empty},{empty}},
     {node,3,{empty},
      {node,3,{empty},{empty}}}}.



-spec ic(btree()) -> boolean().
ic(T) ->
    ic_helper(queue:in(T,queue:new())).

