%% Based on code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-export([start/0, init/0]).

%% These are the start functions used to create and
%% initialize the server.

start() ->
    Pid = spawn(?MODULE, init, []),
    register(?MODULE, Pid),
    Pid.

init() ->
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      {NewFrequencies, Reply} = deallocate(Frequencies, Pid, Freq),
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    {request, Pid, stop} ->
      Pid ! {reply, stopped}
  end.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  maybe_allocate(Freq, Free, Allocated, Pid, lists:keyfind(Pid, 2, Allocated)).

maybe_allocate(NextFreq, Free, Allocated, Pid, false) ->
  {{Free, [{NextFreq, Pid}|Allocated]}, {ok, NextFreq}};
maybe_allocate(NextFreq, Free, Allocated, Pid, {Freq, Pid}) ->
  {{[NextFreq|Free], Allocated}, {error, {client_assigned, Freq}}}.

deallocate({Free, Allocated}, Pid, Freq) ->
  maybe_deallocate(Free, Allocated, Pid, Freq, lists:member({Freq, Pid}, Allocated)).

maybe_deallocate(Free, Allocated, Pid, Freq, true) ->
  NewAllocated=lists:delete({Freq, Pid}, Allocated),
  {{[Freq|Free],  NewAllocated}, ok};
maybe_deallocate(Free, Allocated, _Pid, _Freq, false) ->
  {{Free, Allocated}, {error, no_such_assignment}}.
