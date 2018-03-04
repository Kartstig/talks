-module(kvgs).

-behaviour(gen_server).

-export([start/2, start_link/0]).
-export([code_change/3, init/1, handle_call/3, handle_info/2, terminate/2]).
-export([get/2, put/2]).

-define(PROC, kvstore).

start(normal, []) ->
  kvgs:start_link().

start_link() ->
  gen_server:start_link({local, ?PROC}, ?MODULE, [], []).

init([]) ->
  {ok, #{}}.

get(Key, Default) when is_atom(Key) ->
  gen_server:call(?PROC, {get, Key, Default});
get(Key, Default) ->
  gen_server:call(?PROC, {get, list_to_atom(Key), Default}).

put(Key, Value) when is_atom(Key) ->
  gen_server:call(?PROC, {put, Key, Value});
put(Key, Value) ->
  gen_server:call(?PROC, {put, list_to_atom(Key), Value}).

handle_call({get, Key}, _From, State) ->
  Val = maps:get(Key, State),
  {reply, {ok, Val}, State};
handle_call({get, Key, Default}, _From, State) ->
  Val = maps:get(Key, State, Default),
  {reply, {ok, Val}, State};
handle_call({put, Key, Value}, _From, State) ->
  NewState = maps:put(Key, Value, State),
  {reply, {ok}, NewState}.

handle_info(Msg, State) ->
  io:format("Unexpected message: ~p~n", [Msg]),
  {noreply, State}.

terminate(normal, _State) ->
  io:format("~w shutting down~n", [self()]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

