-module(gs).

-compile(export_all).

%% Demonstrates the hot swapping of code:
%% 
%% S = gs:start(fun (X, S) -> {X * X, S + 1} end, 0).
%% gs:request(S, 10).

start(F, InitialState) ->
    spawn(?MODULE, generic_server, [F, InitialState]).

generic_server(F, State) ->
    receive
        {From, req, Ref, Data} ->
            case catch F(Data, State) of
                {'EXIT', _} ->
                    From ! {self(), err, Ref},
                    generic_server(F, State);
                {Reply, NewState} ->
                    From ! {self(), repl, Ref, Reply},
                    generic_server(F, NewState);
                _ ->
                    From ! {self(), err, Ref},
                    generic_server(F, State)
            end;
        {From, update, Ref, G} ->
            From ! {self(), ok, Ref},
            generic_server(G, State);
        {From, read, Ref} ->
            From ! {self(), repl, Ref, State},
            generic_server(F, State)
    end.

request(S, N) -> 
    R = make_ref(),
    S ! {self(), req, R, N},
    receive 
        {S, req, R, Reply} -> Reply;
        {S, err, R} -> error
    end.