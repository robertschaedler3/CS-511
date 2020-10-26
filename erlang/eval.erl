-module(eval).
-compile(export_all).

%% Exercise 1

%% Syntax of a simple arithmetic language
%%
%% <exp> ::= 
%%    | {const ,<integer >} 
%%    | {add ,<exp >,<exp >} 
%%    | {sub ,<exp >,<exp >} 
%%    | {mul ,<exp >,<exp >} 
%%    | {divi ,<exp >,<exp >}
%%
%% Example:  3 + (4/2)
%%
%% Example of evaluation:
%%
%% > eval:calc(eval:e1()).
%% {val,5}


e1() ->
    {add, 
        {const, 3},
        {divi, 
            {const, 4},
            {const, 2}}}.

calc(Exp) -> 
    implement.



%% Exercise 2

%% Syntax of a simple arithmetic language
%%
%% <exp> ::= 
%%    | {var, <string>}
%%    | {const ,<integer >} 
%%    | {add ,<exp >,<exp >} 
%%    | {sub ,<exp >,<exp >} 
%%    | {mul ,<exp >,<exp >} 
%%    | {divi ,<exp >,<exp >}
%%
%% Example:  3 + (x/2)
%%
%% Example of evaluation:
%%
%% > eval:calc(eval:e2()),[{"x",8}, {"y",2}]).
%% {val,7}

calc(Exp, En) -> 
    implement.
