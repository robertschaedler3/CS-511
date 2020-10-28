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

all_empty(Q) -> 
    case queue:is_empty(Q) of
        true -> true;
        _ -> case queue:out(Q) of 
            {{value, {empty}}, Q2} -> all_empty(Q2);
            _ -> false
        end
    end.

ic_helper(Q) ->
    {{value, T}, Q2} = queue:out(Q),
    case T of
        {empty} -> all_empty(Q2);
        {node,_,LT,RT} -> ic_helper(queue:in(RT, queue:in(LT, Q2)))
    end.

-spec ic(btree()) -> boolean().
ic(T) ->
    ic_helper(queue:in(T,queue:new())).

