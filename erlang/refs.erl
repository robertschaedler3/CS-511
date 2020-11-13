-module(fs).
-compile(export_all).

fact(0)->
    1;
fact(N) when N > 0 ->
    fact(N) * fact(N - 1).

start(N) -> 
    S = spawn(?MODULE, server, []),
    [ spawn(?MODULE, client, [S]) || _ <- lists:seq(1,N) ],
    ok.

server() ->
    receive 
        {From, req, Ref, N} -> 
            From!{self(), repl, Ref, fact(N)},
            server();  
        stop -> ok
    end.

client(S) ->
    R1 = make_ref(),
    R2 = make_ref(),
    S!{self(), req, R1, 10},
    S!{self(), req, R2, 12},
    receive 
        {S, repl, A1} -> 
            receive
                {S, repl, A2} -> 
                    io:format("fact(10) = ~w | fact(12) = ~w", [A1, A2])
            end
    end.
    
