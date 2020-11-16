-module(ex4).

-compile(export_all).

start(N, Frequency) ->
    Clients = [spawn(?MODULE, client, []) || _ <- lists:seq(1, N)],
    spawn(?MODULE, timer, [Frequency, Clients, 0]),
    ok.

client() ->
    receive
        {tick, T} ->
            io:format("~p ~wms~n", [self(), T]),
            client()
    end.

timer(Frequency, Clients, Time) ->
    timer:sleep(Frequency),
    [ C ! {tick, Time + Frequency} || C <- Clients],
    timer(Frequency, Clients, Time + Frequency).
