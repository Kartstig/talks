-module(kvgs_app).

-behaviour(gen_server).

-export([start/2, start_link/0]).
-export([code_change/3, init/1, handle_call/3, handle_info/2, terminate/2]).


start(normal, []) ->
  kvgs_app:start_link().

start_link() ->
  gen_server:start_link({local, kvstore}, ?MODULE, [], []).

init([]) ->
  {ok, #{}}.

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

