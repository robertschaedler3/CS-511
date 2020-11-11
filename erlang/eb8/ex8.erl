-module(ex8).

-compile(export_all).

start() -> 
    S = spawn(fun server/0),
    [ spawn(?MODULE, client, [S]) || _ <- lists:seq(1,10) ].


server() -> 
    receive
        {From, start} -> 
            Serverlet = spawn(?MODULE, serverlet, [rand:uniform(10)]),
            From ! {Serverlet, make_ref(), ok},
            server()
    end.

serverlet(Number) ->
    receive 
        {From, Ref, Guess} when Number /= Guess -> 
            From ! {Ref, tryAgain},
            serverlet(Number);
        {From, Ref, _Guess} -> 
            From ! {Ref, gotIt}
    end.

client(S) -> 
    S ! {self(), start},
    receive {Serverlet, Ref, ok} -> play(Serverlet, Ref) end.
    
play(S, Ref) ->
    Guess = rand:uniform(10),
    io:format("serverlet ~p | client ~p | guess ~w~n", [S, self(), Guess]),
    S ! {self(), Ref, Guess},
    receive 
        {Ref, tryAgain} -> io:format("serverlet ~p | client ~p | try again~n", [S, self()]), play(S, Ref);
        {Ref, gotIt} -> io:format("serverlet ~p | client ~p | got it~n", [S, self()])
    end.