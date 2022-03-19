%%%-------------------------------------------------------------------
%%% @author Piotr Biały
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. kwi 2021 12:27
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("Piotr Biały").

%% API
-import(pollution, [
  createMonitor/0,
  addStation/3,
  addValue/5,
  removeValue/4,
  getOneValue/4,
  getStationMean/3,
  getDailyMean/3,
  getMovingMean/3,
  getNearestStation/2,
  getNearestStationIndex/2
]).

-export([
  start/0,
  stop/0,
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


start() ->
  register(server_pollution, spawn(fun() -> init() end)).

stop() ->
  server_pollution ! stop.

init() ->
  loop(pollution:createMonitor()).

call(Message) ->
  server_pollution ! {request, self(), Message},
  receive
    {reply, Reply} -> Reply
  end.

loop(Monitor) ->
  receive
    {request, Pid, {addStation, Name, Location}} ->
      loop(Pid, addStation(Name, Location, Monitor), Monitor);
    {request, Pid, {addValue, Station, DateTime, Type, Value}} ->
      loop(Pid, addValue(Station, DateTime, Type, Value, Monitor), Monitor);
    {request, Pid, {removeValue, Station, DateTime, Type}} ->
      loop(Pid, removeValue(Station, DateTime, Type, Monitor), Monitor);
    {request, Pid, {getOneValue, Station, DateTime, Type}} ->
      Pid ! {reply, getOneValue(Station, DateTime, Type, Monitor)},
      loop(Monitor);
    {request, Pid, {getStationMean, Station, Type}} ->
      Pid ! {reply, getStationMean(Station, Type, Monitor)},
      loop(Monitor);
    {request, Pid, {getDailyMean, Date, Type}} ->
      Pid ! {reply, getDailyMean(Date, Type, Monitor)},
      loop(Monitor);
    {request, Pid, {getMovingMean, Location, Type}} ->
      Pid ! {reply, getMovingMean(Location, Type, Monitor)},
      loop(Monitor);
    {request, Pid, {getNearestStation, Location}} ->
      Pid ! {reply, getNearestStation(Location, Monitor)},
      loop(Monitor);
    {request, Pid, {getNearestStationIndex, Location}} ->
      Pid ! {reply, getNearestStationIndex(Location, Monitor)},
      loop(Monitor);
    stop -> ok
  end.

loop(Pid, {error, _} = Reply, OldMonitor) ->
  Pid ! {reply, Reply},
  loop(OldMonitor);

loop(Pid, UpdatedMonitor, _) ->
  Pid ! {reply, ok},
  loop(UpdatedMonitor).

addStation(Name, {_Coord1, _Coord2} = Location) ->
  call({addStation, Name, Location}).

addValue(Station, DateTime, Type, Value) ->
  call({addValue, Station, DateTime, Type, Value}).

removeValue(Station, DateTime, Type) ->
  call({removeValue, Station, DateTime, Type}).

getOneValue(Station, DateTime, Type) ->
  call({getOneValue, Station, DateTime, Type}).

getStationMean(Station, Type) ->
  call({getStationMean, Station, Type}).

getDailyMean(Date, Type) ->
  call({getDailyMean, Date, Type}).

getMovingMean({_Coord1, _Coord2} = Location, Type) ->
  call({getMovingMean, Location, Type}).

getNearestStation({_Coord1, _Coord2} = Location) ->
  call({getNearestStation, Location}).

getNearestStationIndex({_Coord1, _Coord2} = Location) ->
  call({getNearestStationIndex, Location}).