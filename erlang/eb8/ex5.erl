-module(ex5).

-compile(export_all).

start(N, Frequency) ->
    S = spawn(?MODULE, timer_server, [[]]),
    spawn(?MODULE, timer, [Frequency, S, 0]),
    register_clients(N, S).

register_clients(N, S) ->
    [ S ! {register, spawn(?MODULE, client, [])} || _ <- lists:seq(1, N)],
    ok.

client() ->
    receive
        {tick, T} ->
            io:format("~p ~wms~n", [self(), T]),
            client()
    end.

timer_server(Clients) -> 
    receive
        {register, Client} -> timer_server([Client | Clients]);
        {tick, Time} -> 
            [C ! {tick, Time} || C <- Clients],
            timer_server(Clients)
    end.

timer(Frequency, Server, Time) ->
    timer:sleep(Frequency),
    Server ! {tick, Time + Frequency},
    timer(Frequency, Server, Time + Frequency).

