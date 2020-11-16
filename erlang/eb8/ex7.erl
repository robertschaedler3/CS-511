-module(ex7).

-compile(export_all).

start(W, M) ->
    S = spawn(?MODULE, server, [0, 0]),
    [spawn(?MODULE, man, [S]) || _ <- lists:seq(1, M)],
    [spawn(?MODULE, woman, [S]) || _ <- lists:seq(1, W)],
    S.

woman(S) -> % Reference to PID of server
    S ! {self(), woman}.

man(S) -> % Reference to PID of server
    Ref = make_ref(),
    S ! {self(), Ref, man},
    receive {S, Ref, ok} -> ok end.

server(Women, Men) ->
    receive
        {From, woman} -> 
            io:format("woman ~p entering~n", [From]),
            server(Women + 1, Men);
        {From, Ref, man} when (Women - (Men * 2)) > 1 ->
            io:format("man ~p entering~n", [From]),
            From ! {self(), Ref, ok},
            server(Women, Men + 1);
        {status} -> 
            io:format("women ~w | men ~w~n", [Women, Men]),
            server(Women, Men)
    end.
