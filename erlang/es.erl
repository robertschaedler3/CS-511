-module(es).
-compile(export_all).

start() -> 
    spawn(?MODULE, server_loop, []),
    ok.

server_loop() ->
    receive
        {From, Msg} ->
            From!Msg,
            server_loop();
        stop ->
            ok
    end.