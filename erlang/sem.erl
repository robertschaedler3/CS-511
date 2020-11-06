-module(sem).
-compile(export_all).


start(Permits) ->
    S = spawn(?MODULE, semaphore, [Permits]),
    spawn(?MODULE, client1, [S]),
    spawn(?MODULE, client2, [S]),
    ok.


semaphore(0) ->
    receive
        {_From, release} -> semaphore(1)
    end;
semaphore(N) when N > 0 ->
    receive
        {From, aquire} -> 
            From!{self(), ok},
            semaphore(N - 1);
        {_From, release} -> 
            semaphore(N+1)
    end.


aquire(S) ->
    S!{self(), aquire},
    receive
        {S, ok} -> ok
    end.

release(S) ->
    S!{self(), release}.


% Print ab only after printing cd.

client1(S) ->
    aquire(S),
    io:format("A~n"),
    io:format("B~n").

client2(S) ->
    io:format("C~n"),
    io:format("D~n"),
    release(S).