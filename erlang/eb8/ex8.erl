-module(ex8).

-compile(export_all).

start() -> spawn(fun server/0).

start_clients(S, N) -> [spawn(?MODULE, client, [S]) || _ <- lists:seq(1, N)].

server() -> 
    receive
        {From, Ref, start} -> 
            Serverlet = spawn(?MODULE, serverlet, [rand:uniform(10)]),
            From ! {Serverlet, Ref, ok},
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
    Ref = make_ref(),
    S ! {self(), Ref, start},
    receive {Serverlet, Ref, ok} -> play(Serverlet) end.
    
play(S) ->
    Ref = make_ref(),
    Guess = rand:uniform(10),
    io:format("~p | guess ~w~n", [self(), Guess]),
    S ! {self(), Ref, Guess},
    receive 
        {Ref, tryAgain} -> io:format("~p | incorrect~n", [self()]), play(S);
        {Ref, gotIt} -> io:format("~p | got it~n", [self()])
    end.