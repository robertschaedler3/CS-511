-module(ex3).

-compile(export_all).

start(N, I) ->
    S = spawn(?MODULE, server, [0]),
    [spawn(?MODULE, client, [S, I]) || _ <- lists:seq(1, N)],
    ok.

client(S, 0) -> S ! {counter};
client(S, N) when N > 0 ->
    S ! {continue},
    client(S, N - 1).

server(N) ->
    receive
        {continue} -> server(N + 1);
        {counter} ->
            io:format("counter ~w~n", [N]),
            server(N)
    end.
