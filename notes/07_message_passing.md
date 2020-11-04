# Message Passing

- No shared memory
  - A process sends a message
  - Another process recieves the message

## Operations

```
recieve(Var);
send(PID, msg);
```

- `recieve` blocks until a message is available in the mailbox

- `send(PID, msg)` is non-blocking; it sends message `msg` to process PID

This model is the *asynchronous communication model and is the one used in Erlang.

## Nodes and Processes in Erlang

A distributed Erlang system consists of a number of Erlang runtime systems communicating with each other (instances ofthe VM). Each runtime is called a `node`.

**Echo server example**
```erlang
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
```



