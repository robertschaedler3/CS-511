-module(ex2).

-compile(export_all).

start() ->
    S = spawn(?MODULE, server, []),
    [spawn(?MODULE, client, [S]) || _ <- lists:seq(1, 10)],
    ok.

client(S) ->
    S ! {start, self()},
    receive 
        {Serverlet, ok} -> 
            Serverlet ! {add, "h", self()},
            Serverlet ! {add, "e", self()},
            Serverlet ! {add, "l", self()},
            Serverlet ! {add, "l", self()},
            Serverlet ! {add, "o", self()},
            Serverlet ! {done, self()},
            receive
                {Serverlet, Str} -> io:format("serverlet ~p | client ~p | msg '~s'~n", [Serverlet, self(), Str])
            end
    end.

server() -> 
    receive
        {start, C_PID} -> 
            C_PID ! {spawn(?MODULE, serverlet, [C_PID, ""]), ok},
            server()
    end.

serverlet(C_PID, State) -> 
    receive
        {add, Str, C_PID} -> serverlet(C_PID, State ++ Str);
        {done, C_PID} -> 
            C_PID ! {self(), State},
            serverlet(C_PID, State)
    end.
        
%% If multiple clients want to use the service, 
%% all messages will be waiting in the server's mailbox.

%% Use serverlets to create concat servers for each new client.