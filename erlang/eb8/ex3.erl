-module(ex3).

-compile(export_all).

start(N, I) ->
    S = spawn(?MODULE, server, [0]),
    [spawn(?MODULE, client, [S, I]) || _ <- lists:seq(1, N)],
    ok.

client(S, I) when I > 0 ->
    S ! {continue, self()},
    client(S, I - 1);
client(S, I) when I == 0 -> S ! {counter, self()}.

server(Count) ->
    receive
        {continue, _C} -> server(Count + 1);
        {counter, _C} ->
            io:format("counter ~w~n", [Count]),
            server(Count)
    end.
