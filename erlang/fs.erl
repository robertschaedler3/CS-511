-module(fs).
-compile(export_all).

fact(0)->
    1;
fact(N) ->
    fact(N) * fact(N - 1).

start(N) -> 
    S = spawn(?MODULE, server, []),
    [ spawn(?MODULE, client, [S, N]) || _ <- lists:seq(1,N) ],
    ok.

server() ->
    receive 
        {From, req, N} -> 
            From!{self(), repl, fact(N)},
            server()
    end.

client(S, N) ->
    S!{self(), req, N},
    receive
        {_, repl, F} -> io:format("~w~s", F)
    end.
