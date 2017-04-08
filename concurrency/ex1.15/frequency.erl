%% Based on code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-export([start/0,allocate/0,deallocate/1,stop/0]).
-export([show_state/0]).
-export([init/0]).

-define(CLIENT_TIMEOUT, 500).

%% These are the start functions used to create and
%% initialize the server.

start() ->
    register(frequency,
             spawn(frequency, init, [])).

init() ->
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      %% Uncomment the sleep for timeout testing only
      %% timer:sleep(?CLIENT_TIMEOUT + 500),
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {frequency_reply, Reply},
      loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      NewFrequencies = deallocate(Frequencies, Freq),
      Pid ! {frequency_reply, ok},
      loop(NewFrequencies);
    {request, Pid , show_state} ->
      {Allocated, Free} = dump(Frequencies),
      Pid ! {frequency_reply, {Allocated, Free}},
      loop(Frequencies);
    {request, Pid, stop} ->
      Pid ! {frequency_reply, stopped}
  end.

%% Functional interface

%% Debugging function to let the client interrogate the server.
show_state() ->
    clear(),
    frequency ! {request, self(), show_state},
    receive
        {frequency_reply, Reply} -> Reply
    after ?CLIENT_TIMEOUT ->
            {error, timeout}
    end.

%% Among other flaws, the server does not limit an individual client
%% to a single frequency.
allocate() ->
    clear(),
    frequency ! {request, self(), allocate},
    receive
        {frequency_reply, Reply} -> Reply
    after ?CLIENT_TIMEOUT ->
            {error, timeout}
    end.

deallocate(Freq) ->
    clear(),
    frequency ! {request, self(), {deallocate, Freq}},
    receive
            {frequency_reply, Reply} -> Reply
    after ?CLIENT_TIMEOUT ->
            %% Timeout on deallocation is tricky because now we're in
            %% an unknown state. Will the server ever receive and
            %% process the message? Impossible to know for certain.
            %%
            %% Clients should always assume the frequency is no longer
            %% available for their use.
            {error, timeout}
    end.

stop() ->
    frequency ! {request, self(), stop},
    receive
            {frequency_reply, Reply} -> Reply
    end.


%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

dump({Free, Allocated}) ->
    {{free, Free}, {allocated, Allocated}}.

%% It is confusing to have a function with different arities used for
%% both the client API and as server-side helpers. I would definitely
%% rename `allocate/2' and `deallocate/2' in a production system.
allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
  NewAllocated=lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free],  NewAllocated}.

%% Arbitrary clearing out all client mailbox messages when invoking an
%% API is a terrible idea for production code. To make this more
%% palatable I have modified all `reply' tagged tuples to be
%% `frequency_reply' so that I could match only those messages I'm
%% confident are related to this library.
%%
%% For a production system I would probably identify all allocation
%% messages which were ignored and send corresponding deallocation
%% messages to the server before attempting another allocation.
clear() ->
    receive
        {frequency_reply, _} = _Msg ->
            %% Uncomment for testing only
            %% io:format("Dropped message: ~p~n", [_Msg]),
            clear()
    after 0 ->
            ok
    end.
