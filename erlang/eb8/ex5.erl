-module(ex5).

-compile(export_all).

start(N, Interval) ->
    S = spawn(?MODULE, timer_server, [[]]),
    spawn(?MODULE, timer, [Interval, S, 0]),
    register_clients(N, S).

register_clients(N, S) ->
    lists:foreach(fun (_) -> 
        S ! {register, spawn(?MODULE, client, [])} end,
        lists:seq(1, N)
    ),
    ok.

client() ->
    receive
        {tick, T} ->
            io:format("~p tick recieved ~w~n", [self(), T]),
            client()
    end.

timer_server(Clients) -> 
    receive
        {register, Client} -> timer_server([Client | Clients]);
        {tick, Time} -> 
            lists:foreach(fun (C) -> C ! {tick, Time} end, Clients),
            timer_server(Clients)
    end.

timer(Interval, Server, Time) ->
    timer:sleep(Interval),
    Server ! {tick, Time + Interval},
    timer(Interval, Server, Time + Interval).

