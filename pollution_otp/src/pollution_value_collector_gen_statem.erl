%%%-------------------------------------------------------------------
%%% @author Piotr Biały
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. maj 2021 15:32
%%%-------------------------------------------------------------------
-module(pollution_value_collector_gen_statem).
-author("Piotr Biały").

-behaviour(gen_statem).
%% API
-export([init/1, callback_mode/0, start_link/0, stop/0]).
-export([set_station/1, add_value/3, store_data/0]).
-export([create/3, collect/3]).


init([]) ->
  {ok, create, []}.

callback_mode() ->
  state_functions.

start_link() ->
  gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_statem:stop(?MODULE).

set_station(Name) ->
  gen_statem:cast(?MODULE, {setStation, Name}).

add_value(DateTime, Type, Value) ->
  gen_statem:cast(?MODULE, {addValue, DateTime, Type, Value}).

store_data() ->
  gen_statem:cast(?MODULE, flushData).

create(_Event, {setStation, Name}, _) ->
  {next_state, collect, {Name, []}}.

collect(_Event, {addValue, DateTime, Type, Value}, {Name, [Measurements]}) ->
  {next_state, collect, {Name, [{DateTime, Type, Value} | Measurements]}};

collect(_Event, flushData, {Name, Measurements}) ->
  lists:foreach(fun({DateTime, Type, Value}) -> pollution_gen_server:addValue(Name, DateTime, Type, Value) end, Measurements),
  {next_state, create, []}.
