-module(palin_server).
-export([start/0, init/0]).

-define(IS_P_FMT, "\"~ts\" is a palindrome").
-define(NOT_P_FMT, "\"~ts\" is NOT a palindrome").

start() ->
    spawn(?MODULE, init, []).

init() ->
    listen_loop().

listen_loop() ->
    receive
        {check, Client, String} ->
            test_palindrome(Client, String),
            listen_loop();
        _ ->
            ok
    end.

test_palindrome(Pid, String) ->
    respond(Pid, String, result_string(String, palin:palindrome(String))).

respond(Pid, String, Result) ->
    Pid ! {result, String, Result}.

result_string(String, true) ->
    lists:flatten(io_lib:format(?IS_P_FMT, [String]));
result_string(String, false) ->
    lists:flatten(io_lib:format(?NOT_P_FMT, [String])).
