-module(ex6).

-compile(export_all).

start(N) ->
    S = spawn(?MODULE, server, []),
    [spawn(?MODULE, client, [S, I]) || I <- lists:seq(1, N)],
    ok.

is_prime(N) -> 
    case try lists:foreach(
        fun(X) -> 
            case math:fmod(N, X) == 0 of
                true -> throw(not_prime);
                false -> do_nothing
            end
        end,
        lists:seq(2, round(math:sqrt(N)))) 
    catch 
        throw:not_prime -> false
    end of 
        false -> false;
        _ -> true
    end.

client(S, N) ->
    S ! {is_prime, N, self()},
    receive 
        {result, R} -> io:format("~p ~w is prime: ~w~n", [self(), N, R])
    end.

server() ->
    receive
        {is_prime, N, C} -> 
            C ! {result, is_prime(N)},
            server()
    end.
