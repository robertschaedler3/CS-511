-module(barr).

-compile(export_all).

start(N) ->
    B = spawn(?MODULE, coordinator, [N, N, []]),
    [spawn(?MODULE, client, [B, 1]) || _ <- lists:seq(1, N)],
    [spawn(?MODULE, client, [B, 2]) || _ <- lists:seq(1, N)],
    ok.

%% N is the total number of threads
%% M is how many threads that still have to reach the barrier
%% L is the list of the PIDs that have already reached the barrier
coordinator(N, 0, L) ->
    [From ! {self(), ok, Ref} || {From, Ref} <- L],
    coordinator(N, N, []);
coordinator(N, M, L) when M > 0 ->
    receive
        {From, arrived, Ref} ->
            coordinator(N, M - 1, [{From, Ref} | L])
    end.

barrier(B) ->
    R = make_ref(),
    B ! {self(), arrived, R},
    receive {B, ok, R} -> ok end.

client(B, N) ->
    io:format("client ~p | ~w waiting~n", [self(), N]),
    barrier(B),
    io:format("client ~p | ~w passed~n", [self(), N]).
