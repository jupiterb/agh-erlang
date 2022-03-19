%%%-------------------------------------------------------------------
%%% @author Piotr Biały
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. kwi 2021 16:03
%%%-------------------------------------------------------------------
-module(pollution_server_test).
-author("Piotr Biały").

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

% basic tests

start_stop_test() ->
  pollution_server:start(),
  ?assert(lists:member(server_pollution, registered())),
  pollution_server:stop(),
  timer:sleep(100),
  ?assert(not lists:member(server_pollution, registered())).

addStation_test()->
  pollution_server:start(),
  ?assertEqual(ok, pollution_server:addStation("Krakow", {50, 50})),
  ?assertMatch({error, _}, pollution_server:addStation("Poznan", {50, 50})),
  pollution_server:stop().

addValue_test() ->
  pollution_server:start(),
  pollution_server:addStation("Krakow", {50, 50}),
  ?assertEqual(ok, pollution_server:addValue("Krakow", {{2021,4,11},{12,20,1}}, "PM10", 70)),
  ?assertEqual(ok, pollution_server:addValue("Krakow", {{2021,4,11},{12,20,1}}, "PM2.5", 70)),
  ?assertEqual({error, station_not_exist},
    pollution_server:addValue({30, 30}, {{2021,4,11},{13,20,1}}, "PM10", 50)),
  ?assertEqual({error, measurement_exist},
    pollution_server:addValue({50, 50}, {{2021,4,11},{12,20,1}}, "PM10", 50)),
  pollution_server:stop().

todaySubtractHour(Hour) ->
  {{Ye, Mo, Da} , {Ho, Min, Sec}} = calendar:local_time(),
  {{Ye, Mo, Da} , {Ho - Hour, Min, Sec}}.

prepareServer() ->
  pollution_server:start(),
  pollution_server:addStation("Poznan", {10, 10}),
  pollution_server:addStation("Krakow", {20, 20}),
  pollution_server:addValue("Krakow", todaySubtractHour(0), "PM10", 80),
  pollution_server:addValue("Krakow", todaySubtractHour(1), "PM10", 70),
  pollution_server:addValue("Krakow", todaySubtractHour(2), "PM10", 60),
  pollution_server:addValue("Krakow", todaySubtractHour(0), "PM2.5", 50),
  pollution_server:addValue("Poznan", todaySubtractHour(2), "PM10", 60),
  pollution_server:addValue("Poznan", todaySubtractHour(0), "PM2.5", 30),
  pollution_server:addValue("Poznan", todaySubtractHour(1), "PM2.5", 40).

getOneValue_test() ->
  prepareServer(),
  ?assertEqual(70, pollution_server:getOneValue({20, 20}, todaySubtractHour(1), "PM10")),
  ?assertEqual(40, pollution_server:getOneValue("Poznan", todaySubtractHour(1), "PM2.5")),
  pollution_server:stop().

removeValue_test() ->
  prepareServer(),
  ?assertEqual(ok, pollution_server:removeValue("Krakow", todaySubtractHour(0), "PM10")),
  ?assertMatch({error, _}, pollution_server:removeValue("Krakow", todaySubtractHour(0), "PM10")),
  pollution_server:stop().

getStationMean_test() ->
  prepareServer(),
  ?assertEqual(70.0, pollution_server:getStationMean("Krakow", "PM10")),
  ?assertEqual(35.0, pollution_server:getStationMean({10,10}, "PM2.5")),
  ?assertMatch({error, _}, pollution_server:getStationMean({10,10}, "Temperature")),
  pollution_server:stop().

getDailyMean_test() ->
  prepareServer(),
  {Date, _} = calendar:local_time(),
  ?assertEqual(40.0, pollution_server:getDailyMean(Date, "PM2.5")),
  ?assertEqual(67.5, pollution_server:getDailyMean(Date, "PM10")),
  {{Ye, Mo, Da}, _} = calendar:local_time(),
  Tomorrow = {Ye, Mo, Da+1},
  ?assertMatch({error, _}, pollution_server:getDailyMean(Tomorrow, "PM10")),
  pollution_server:stop().

getMovingMean_test() ->
  prepareServer(),
  ?assertEqual(true, pollution_server:getMovingMean({10,10}, "PM2.5") < 35.0),
  pollution_server:stop().

getNearestStation_test() ->
  prepareServer(),
  ?assertEqual({20, 20}, pollution_server:getNearestStation({20, 30})),
  ?assertEqual({10, 10}, pollution_server:getNearestStation({5, 5})),
  pollution_server:addStation("Gdansk", {5, 10}),
  ?assertEqual({5, 10}, pollution_server:getNearestStation({5, 5})),
  pollution_server:stop().

getNearestStationIndex_test() ->
  prepareServer(),
  ?assertEqual(1.0, pollution_server:getNearestStationIndex({10, 12})),
  ?assertEqual(1.6666666666666667, pollution_server:getNearestStationIndex({22, 18})),
  pollution_server:stop().
