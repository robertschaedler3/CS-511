-module(ex1).
-compile(export_all).

start(N) -> %% Spawns a counter and N turnstile clients
    C = spawn(?MODULE, counter_server, [0]),
    [spawn(?MODULE, turnstile, [C, 50]) || _ <- lists:seq(1, N)],
    C.

counter_server(State) -> %% State is the current value of the counter
    receive
        {_From, bump, N} -> 
            counter_server(N + State);
        {From, read} -> 
            From!{State},
            counter_server(State)
    end.

turnstile(C, N) -> %% C is the PID of the counter, and N the number of turns
    C!{self(), bump, N}.
