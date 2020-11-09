-module(ex1).

-compile(export_all).

start(N) -> %% Spawns a counter and N turnstile clients
    S = spawn(?MODULE, counter_server, [0]),
    [spawn(?MODULE, turnstile, [S, 50]) || _ <- lists:seq(1, N)],
    S.

counter_server(N) -> %% State is the current value of the counter
    receive
        {bump} -> counter_server(N + 1);
        {From, read} ->
            From ! {N},
            counter_server(N)
    end.

turnstile(_S, 0) -> 
    ok;
turnstile(S, N) when N > 0 -> 
    S ! {bump},
    turnstile(S, N - 1).
