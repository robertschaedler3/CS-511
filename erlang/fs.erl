-module(fs).

-compile(export_all).

fact(0) -> 1;
fact(N) -> N * fact(N - 1).

start(N) ->
    S = spawn(?MODULE, server, []),
    [spawn(?MODULE, client, [S, X]) || X <- lists:seq(1, N)],
    ok.

server() ->
    receive
        {From, req, N} ->
            From ! {self(), repl, fact(N)},
            server()
    end.

client(S, X) ->
    S ! {self(), req, X},
    receive
        {_, repl, F} -> io:format("~p fact(~w) = ~w~n", [self(), X, F])
    end.
