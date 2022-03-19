%%%-------------------------------------------------------------------
%%% @author Piotr Biały
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. kwi 2021 17:51
%%%-------------------------------------------------------------------
-module(pollution).
-author("Piotr Biały").

%% API
-export([
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

-record(monitor, {stations, measurements}).

% monitor is a record consisting of two maps:
% stations      {coords => name or name => coords}
%    so we can get name by coords and coords by name
% measurements  {{name, date, measurement type} => value}
%    so it is easy to check if there is such a measurement

%% Required functionalities

createMonitor() ->
  #monitor{stations = #{}, measurements = #{}}.

addStation(Name, {_Coord1, _Coord2} = Location, #monitor{stations = Stations, measurements = Measurements}) ->
  case maps:is_key(Location, Stations) orelse maps:is_key(Name, Stations) of
    true -> {error, station_exist};
    false -> #monitor{
      stations = Stations#{Location => Name, Name => Location},
      measurements = Measurements
    }
  end.

addValue({_Coord1, _Coord2} = Location, DateTime, Type, Value, Monitor = #monitor{stations = Stations}) ->
  case maps:is_key(Location, Stations) of
    true -> addValue({maps:get(Location, Stations), validDateTime(DateTime), Type}, Value, Monitor);
    false -> {error, station_not_exist}
  end;

addValue(Name, DateTime, Type, Value, Monitor = #monitor{stations = Stations}) ->
  case maps:is_key(Name, Stations) of
    true -> addValue({Name, validDateTime(DateTime), Type}, Value, Monitor);
    false -> {error, station_not_exist}
  end.

removeValue({_Coord1, _Coord2} = Location, DateTime, Type, Monitor = #monitor{stations = Stations}) ->
  removeValue(maps:get(Location, Stations), DateTime, Type, Monitor);

removeValue(Name, DateTime, Type, #monitor{stations = Stations, measurements = Measurements}) ->
  ValidDateTime = validDateTime(DateTime),
  case maps:is_key({Name, ValidDateTime, Type}, Measurements) of
    true -> #monitor{stations = Stations, measurements = maps:remove({Name, ValidDateTime, Type}, Measurements)};
    false -> {error, measurement_not_exist}
  end.

getOneValue({_Coord1, _Coord2} = Location, DateTime, Type, Monitor = #monitor{stations = Stations}) ->
  getOneValue(maps:get(Location, Stations), DateTime, Type, Monitor);

getOneValue(Name, DateTime, Type, #monitor{measurements = Measurements}) ->
  maps:get({Name, validDateTime(DateTime), Type}, Measurements).

getStationMean({_Coord1, _Coord2} = Location, Type, Monitor = #monitor{stations = Stations}) ->
  getStationMean(maps:get(Location, Stations), Type, Monitor);

getStationMean(Name, Type, #monitor{measurements = Measurements}) ->
  Pred = fun ({KeyName, _, KeyType}, _) -> KeyName == Name andalso KeyType == Type end,
  mean(maps:values(maps:filter(Pred, Measurements))).

getDailyMean(Date, Type, #monitor{measurements = Measurements}) ->
  Pred = fun ({_, {KeyDate, _}, KeyType}, _) -> KeyType == Type andalso KeyDate == Date end,
  mean(maps:values(maps:filter(Pred, Measurements))).

%% Auxiliary functionalities

addValue(Key, Value, #monitor{stations = Stations, measurements = Measurements}) ->
  case maps:is_key(Key, Measurements) of
    true -> {error, measurement_exist};
    false -> #monitor{
      stations = Stations,
      measurements = maps:put(Key, Value, Measurements)
    }
  end.

validDateTime({Date, {Hour, _, _}}) -> {Date, {Hour, 0, 0}}.

mean([]) -> {error, no_readings};
mean(Values) -> lists:sum(Values) / length(Values).

%% Selected functionality from the list (13) - returns the moving average of the parameter value
%% of a given type on a given day at the station with the given coordinates
%% (it is a weighted average from the measurements from the last day, t
%% he current value should be taken into account with the weight of 24,
%% the value from an hour ago with the weight of 23, and so on until up to 23 hours ago, weighing 1);

getMovingMean({_Coord1, _Coord2} = Location, Type, #monitor{stations = Stations, measurements = Measurements}) ->
  Name = maps:get(Location, Stations),
  Pred = fun({KeyName, KeyDateTime, KeyType}, _) ->
      KeyName == Name andalso
      KeyType == Type andalso
      isLastDay(timeDiff(KeyDateTime)) end,
  movingMean(maps:to_list(maps:filter(Pred, Measurements)), 0, 0).

timeDiff(DateTime) -> calendar:time_difference(DateTime, calendar:local_time()).
isLastDay({0, _}) -> true;
isLastDay(_) -> false.
hours({_, {Hours, _, _}}) -> Hours.

movingMean([], _, 0) ->
  {error, measurement_not_exist};
movingMean([{{_, DateTime, _}, Value} | Tail], ValueAcc, WeightAcc) ->
  movingMean( Tail,
              ValueAcc + Value * (24 - hours(timeDiff(DateTime))),
              WeightAcc + (24 - hours(timeDiff(DateTime))));
movingMean([], ValueAcc, WeightAcc) ->
  ValueAcc / WeightAcc.

%% Additional extensions

% getNearestStation/2 - for the given coordinates it returns the coordinates of the nearest station.
% For simplicity, I took the Euclidean norm as a distance

getNearestStation({_Coord1, _Coord2} = Location, #monitor{stations = Stations}) ->
  Fun = fun
          ({_X1, _X2} = AnotherLocation, _, {_Y1, _Y2} = NearestLocation) ->
            case squareNorm(AnotherLocation, Location) < squareNorm(NearestLocation, Location) of
              true -> AnotherLocation;
              false -> NearestLocation
            end;
          (_, _, NearestLocation) -> NearestLocation
   end,
  maps:fold(Fun, {most, distant}, Stations).

squareNorm({most, distant}, _) -> 134217728; % we need initial value to be maximum - we want to reduce acc
squareNorm({X1, X2}, {Y1, Y2}) -> math:sqrt((X1 - Y1) * (X1 - Y1) + (X2 - Y2) * (X2 - Y2)).

% getNearestStationIndex/2 - for the given coordinates
% it returns a simple air quality index at the nearest measuring station

getNearestStationIndex({_Coord1, _Coord2} = Location, Monitor = #monitor{stations = Stations, measurements = Measurements}) ->
  Pred = fun ({KeyName, KeyDateTime, _}, _) ->
      KeyName == maps:get(getNearestStation(Location, Monitor), Stations) andalso
      KeyDateTime == validDateTime(calendar:local_time()) end,
  Fun = fun ({_, _, Type}, Value, Acc) -> max(getTypeIndex(Type, Value), Acc) end,
  maps:fold(Fun, 0, maps:filter(Pred, Measurements)).

getTypeIndex("PM10", Value) -> Value / 50;
getTypeIndex("PM2.5", Value) -> Value / 30;
getTypeIndex(_, _) -> 0.
