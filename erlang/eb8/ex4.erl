-module(ex4).

-compile(export_all).

start(N, Interval) ->
    Clients = [spawn(?MODULE, client, []) || _ <- lists:seq(1, N)],
    spawn(?MODULE, timer, [Interval, Clients, 0]),
    ok.

client() ->
    receive
        {tick, T} ->
            io:format("~p tick recieved ~w~n", [self(), T]),
            client()
    end.

timer(Interval, Clients, Time) ->
    timer:sleep(Interval),
    lists:foreach(fun (C) -> C ! {tick, Time + Interval} end,
                  Clients),
    timer(Interval, Clients, Time + Interval).
