-module(ex2).

-compile(export_all).

start(N) ->
    Serverlets = [spawn(?MODULE, server, [""]) || _ <- lists:seq(1, N)],
    [spawn(?MODULE, client, [S]) || S <- Serverlets],
    ok.

client(S) ->
    S ! {start, self()},
    S ! {add, "h", self()},
    S ! {add, "e", self()},
    S ! {add, "l", self()},
    S ! {add, "l", self()},
    S ! {add, "o", self()},
    S ! {done, self()},
    receive
        {S, Str} -> io:format("serverlet ~p | client ~p | msg ~s~n", [S, self(), Str])
    end.

server(Str) -> 
    receive
        {add, Str_Msg, _C} -> server(Str ++ Str_Msg);
        {done, C} -> C ! {self(), Str}
    end.

