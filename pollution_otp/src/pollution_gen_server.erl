%%%-------------------------------------------------------------------
%%% @author Piotr Biały
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. maj 2021 11:40
%%%-------------------------------------------------------------------
-module(pollution_gen_server).
-author("Piotr Biały").

-behaviour(gen_server).
%% API
% gen_server api
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
% pollution api
-export([
  start_link/0,
  crash/0,
  close/0,
  addStation/2,
  addValue/4,
  removeValue/3,
  getOneValue/3,
  getStationMean/2,
  getDailyMean/2,
  getMovingMean/2,
  getNearestStation/1,
  getNearestStationIndex/1
]).

% start

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
  {ok, pollution:createMonitor()}.

% handlers

handle_cast({addStation, Name, Location}, Monitor) ->
  handle_helper(pollution:addStation(Name, Location, Monitor), Monitor);

handle_cast({addValue, Station, DateTime, Type, Value}, Monitor) ->
  handle_helper(pollution:addValue(Station, DateTime, Type, Value, Monitor), Monitor);

handle_cast({removeValue, Station, DateTime, Type}, Monitor) ->
  handle_helper(pollution:removeValue(Station, DateTime, Type, Monitor), Monitor);

handle_cast(crash, Monitor) ->
  _ = 0/0,
  {noreply, Monitor}.


handle_call({getOneValue, Station, DateTime, Type}, _From, Monitor) ->
  {reply, pollution:getOneValue(Station, DateTime, Type, Monitor), Monitor};

handle_call({getStationMean, Station, Type}, _From, Monitor) ->
  {reply, pollution:getStationMean(Station, Type, Monitor), Monitor};

handle_call({getDailyMean, Date, Type}, _From, Monitor) ->
  {reply, pollution:getDailyMean(Date, Type, Monitor), Monitor};

handle_call({getMovingMean, Location, Type}, _From, Monitor) ->
  {reply, pollution:getMovingMean(Location, Type, Monitor), Monitor};

handle_call({getNearestStation, Location}, _From, Monitor) ->
  {reply, pollution:getNearestStation(Location, Monitor), Monitor};

handle_call({getNearestStationIndex, Location}, _From, Monitor) ->
  {reply, pollution:getNearestStationIndex(Location, Monitor), Monitor};

handle_call(terminate, _From, Monitor) ->
  {stop, normal, ok, Monitor}.

% helpers

handle_helper({error, _}, OldMonitor) ->
  {noreply, OldMonitor};

handle_helper(NewMonitor, _) ->
  {noreply, NewMonitor}.

% client interface

crash() ->
  gen_server:cast(?MODULE, crash).

close() ->
  gen_server:call(?MODULE, terminate).


addStation(Name, {_Coord1, _Coord2} = Location) ->
  gen_server:cast(?MODULE, {addStation, Name, Location}).

addValue(Station, DateTime, Type, Value) ->
  gen_server:cast(?MODULE, {addValue, Station, DateTime, Type, Value}).

removeValue(Station, DateTime, Type) ->
  gen_server:cast(?MODULE, {removeValue, Station, DateTime, Type}).


getOneValue(Station, DateTime, Type) ->
  gen_server:call(?MODULE, {getOneValue, Station, DateTime, Type}).

getStationMean(Station, Type) ->
  gen_server:call(?MODULE, {getStationMean, Station, Type}).

getDailyMean(Date, Type) ->
  gen_server:call(?MODULE, {getDailyMean, Date, Type}).

getMovingMean({_Coord1, _Coord2} = Location, Type) ->
  gen_server:call(?MODULE, {getMovingMean, Location, Type}).

getNearestStation({_Coord1, _Coord2} = Location) ->
  gen_server:call(?MODULE, {getNearestStation, Location}).

getNearestStationIndex({_Coord1, _Coord2} = Location) ->
  gen_server:call(?MODULE, {getNearestStationIndex, Location}).

% terminate

terminate(Reason, _) ->
  io:format("Server closed~n"),
  ok.