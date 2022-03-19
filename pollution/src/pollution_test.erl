%%%-------------------------------------------------------------------
%%% @author Piotr Biały
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. kwi 2021 11:32
%%%-------------------------------------------------------------------
-module(pollution_test).
-author("Piotr Biały").

-include_lib("eunit/include/eunit.hrl").

% basic tests

createMonitor_test() ->
  ?assertEqual({monitor, #{}, #{}}, pollution:createMonitor()).


addStation1_test() ->
  M = pollution:createMonitor(),
  {_, Stations, _} = pollution:addStation("Krakow", {50, 50}, M),
  ?assertEqual(Stations, #{"Krakow" => {50, 50}, {50, 50} => "Krakow"}).

addStation2_test() ->
  M = pollution:createMonitor(),
  M1 = pollution:addStation("Krakow", {50, 50}, M),
  M2 = pollution:addStation("Krakow", {60, 60}, M1),
  ?assertEqual(M2, {error, station_exist}).


addValue1_test() ->
  M = pollution:addStation("Krakow", {50, 50}, pollution:createMonitor()),
  {_, _, Measurements} = pollution:addValue("Krakow", {{2021,4,11},{12,20,1}}, "PM10", 70, M),
  ?assertEqual(Measurements, #{{"Krakow", {{2021,4,11},{12,0,0}}, "PM10"} => 70}).

addValue2_test() ->
  M = pollution:addStation("Krakow", {50, 50}, pollution:createMonitor()),
  {_, _, Measurements} = pollution:addValue({50,50}, {{2021,4,11},{12,20,1}}, "PM2.5", 50, M),
  ?assertEqual(Measurements, #{{"Krakow", {{2021,4,11},{12,0,0}}, "PM2.5"} => 50}).

% more complex tests

todaySubtractHour(Hour) ->
  {{Ye, Mo, Da} , {Ho, Min, Sec}} = calendar:local_time(),
  {{Ye, Mo, Da} , {Ho - Hour, Min, Sec}}.

prepareMonitor() ->
  pollution:addValue("Krakow", todaySubtractHour(0), "PM10", 80,
    pollution:addValue("Krakow", todaySubtractHour(1), "PM10", 70,
      pollution:addValue("Krakow", todaySubtractHour(0), "PM2.5", 50,
        pollution:addValue("Poznan", todaySubtractHour(2), "PM10", 60,
          pollution:addValue("Poznan", todaySubtractHour(0), "PM2.5", 30,
            pollution:addStation("Poznan", {10, 10},
              pollution:addStation("Krakow", {20, 20},
                pollution:createMonitor()))))))).


addValue3_test() ->
  ?assertEqual({error, station_not_exist},
    pollution:addValue({30, 30}, todaySubtractHour(0), "PM10", 50, prepareMonitor())).

addValue4_test() ->
  ?assertEqual({error, measurement_exist},
    pollution:addValue("Krakow", todaySubtractHour(0), "PM10", 50, prepareMonitor())).

getOneValue_test() ->
  ?assertEqual(70, pollution:getOneValue({20, 20}, todaySubtractHour(1), "PM10", prepareMonitor())).

removeValue1_test() ->
  {_, _, Measurements} = pollution:removeValue("Krakow", todaySubtractHour(0), "PM10", prepareMonitor()),
  ?assertEqual(false, maps:is_key({"Krakow", todaySubtractHour(0), "PM10"}, Measurements)).

removeValue2_test() ->
  ?assertEqual({error, measurement_not_exist},
    pollution:removeValue("Warszawa", todaySubtractHour(0), "PM10", prepareMonitor())).

getStationMean1_test() ->
  ?assertEqual(75.0, pollution:getStationMean("Krakow", "PM10", prepareMonitor())).

getStationMean2_test() ->
  ?assertEqual({error, no_readings},
    pollution:getStationMean("Krakow", "Temperature", prepareMonitor())).

getDailyMean1_test() ->
  {Date, _} = calendar:local_time(),
  ?assertEqual(40.0, pollution:getDailyMean(Date, "PM2.5", prepareMonitor())).

getDailyMean2_test() ->
  {{Ye, Mo, Da}, _} = calendar:local_time(),
  Date = {Ye, Mo, Da+1},
  ?assertEqual({error, no_readings},
    pollution:getStationMean(Date, "PM2.5", prepareMonitor())).

getMovingMean_test() ->
  ?assertEqual(true, pollution:getMovingMean({20,20}, "PM10", prepareMonitor()) > 75.0).

getNearestStation_test() ->
  ?assertEqual({20, 20}, pollution:getNearestStation({20, 30}, prepareMonitor())).

getNearestStationIndex1_test() ->
  ?assertEqual(1.6666666666666667, pollution:getNearestStationIndex({20, 30}, prepareMonitor())).

getNearestStationIndex2_test() ->
  ?assertEqual(1.0, pollution:getNearestStationIndex({10, 12}, prepareMonitor())).
