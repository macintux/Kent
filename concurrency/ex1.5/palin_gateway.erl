-module(palin_gateway).
-export([start/1, init/1]).

-define(DEFAULT_GATEWAY_ALGORITHM, random).

%% This could evolve into a more complex structure including weights
%% and current status
-type server() :: pid().

%% The gateway algorithms take the current round robin server index
%% and list of servers, and return the next round robin index and a
%% list of servers to contact (typically just a single server, but the
%% `all' algorithm indicates all servers should be used)
-type algorithm_function() :: fun((pos_integer(), list(server())) ->
                                         {pos_integer(), list(server())}).


-record(gateway_state, {
          rr_idx = 1 :: pos_integer(),
          servers = [] :: list(server()),
          self = self() :: pid()
         }).

start(Pids) ->
    spawn(?MODULE, init, [Pids]).

init(Pids) ->
    %% For production use a non-constant seed is desirable
    _ = rand:seed(exs1024, {1, 1, 1}),
    gateway_loop(#gateway_state{servers=Pids}).

gateway_loop(#gateway_state{self=Gateway, rr_idx=RRIdx, servers=Servers}=State) ->
    receive
        {check, Client, String, Flags} ->
            {ResponseDestination, Algorithm} =
                pick_gateway_option(Gateway, Client, Flags),
            {NewRRIdx, NewServers} =
                execute_gateway_algorithm(Gateway, ResponseDestination, Client,
                                          RRIdx, Algorithm, Servers, String,
                                          lists:member(debug, Flags)),
            gateway_loop(State#gateway_state{rr_idx=NewRRIdx, servers=NewServers});
        _ ->
            ok
    end.

%% If the flag `first' is supplied, the gateway is expected to
%% coordinate responses and send the first result to the
%% client. Otherwise, the client will receive any and all responses.
%%
%% In a more complex system using `first' would also prompt the
%% gateway to deal appropriately with the possibility that all servers
%% are unavailable.
-spec pick_gateway_option(pid(), pid(), list()) -> {pid(), algorithm_function()}.
pick_gateway_option(Gateway, Client, Flags) ->
    ResponsePid = pick_response_pid(Gateway, Client, lists:member(first, Flags)),
    Algorithm = pick_algorithm_fun(Flags),
    {ResponsePid, Algorithm}.

-spec execute_gateway_algorithm(pid(), pid(), pid(), pos_integer(),
                                algorithm_function(), list(server()), string(), boolean()) ->
                                       {pos_integer(), list(server())}.
execute_gateway_algorithm(Gateway, ResponseDestination, Client,
                          RRIdx, Fun, Servers, String, Debug) ->
    {NewIdx, ContactServers} = Fun(RRIdx, Servers),
    maybe_debug_msg(Client, String, ContactServers, Debug),
    lists:foreach(fun(S) -> extract_pid(S) !
                                {check, ResponseDestination, String}
                  end,
                  ContactServers),
    handle_responses(Client, Gateway, ResponseDestination, String),
    {NewIdx, Servers}.

maybe_debug_msg(Pid, String, Servers, true) ->
    Pid ! {debug, String, Servers};
maybe_debug_msg(_Pid, _String, _Servers, false) ->
    ok.

%% When the gateway pid and the response destination pid are the same
%% value, the gateway receives all responses and sends the first to
%% the client.
%%
%% Otherwise, all responses are sent directly from the servers to the
%% client.
handle_responses(Client, Gateway, Gateway, String) ->
    receive
        {result, String, _Response}=Msg ->
            Client ! Msg,
            %% Clean out any other mailbox items for this string
            drop_dups(String)
    after 1000 ->
            Client ! {error, no_responses}
    end;
handle_responses(_Client, _Gateway, _Destination, _String) ->
    ok.

drop_dups(String) ->
    receive
        {result, String, _} ->
            drop_dups(String)
    after 100 ->
            ok
    end.

%% Allow for more complex server data.
-spec extract_pid(server()) -> pid().
extract_pid(Server) ->
    Server.

pick_response_pid(Gateway, _Client, true) ->
    Gateway;
pick_response_pid(_Gateway, Client, false) ->
    Client.

pick_algorithm_fun(Flags) ->
    assemble_algorithm_fun(safe_head(drop_other_flags(Flags))).

drop_other_flags(Flags) ->
    lists:filter(fun(F) -> F /= first andalso F /= debug end, Flags).

safe_head([]) ->
    undefined;
safe_head(List) ->
    hd(List).

assemble_algorithm_fun(undefined) ->
    assemble_algorithm_fun(?DEFAULT_GATEWAY_ALGORITHM);
assemble_algorithm_fun(all) ->
    fun(Idx, Servers) -> {Idx, Servers} end;
assemble_algorithm_fun(random) ->
    fun pick_random/2;
assemble_algorithm_fun(round_robin) ->
    fun round_robin/2;
assemble_algorithm_fun(Unknown) ->
    throw({unknown_algorithm, Unknown}).

pick_random(Idx, Servers) ->
    {Idx, [lists:nth(rand:uniform(length(Servers)), Servers)]}.

round_robin(Idx, Servers) ->
    {wrap(Idx+1, length(Servers)), [lists:nth(Idx, Servers)]}.

wrap(N, Top) when N > Top ->
    N - Top;
wrap(N, _Top) ->
    N.
