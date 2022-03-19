%%%-------------------------------------------------------------------
%%% @author Piotr Biały
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. maj 2021 14:00
%%%-------------------------------------------------------------------
-module(pollution_gen_server_test).
-author("Piotr Biały").

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).


todaySubtractHour(Hour) ->
  {{Ye, Mo, Da} , {Ho, Min, Sec}} = calendar:local_time(),
  {{Ye, Mo, Da} , {Ho - Hour, Min, Sec}}.

prepareServer() ->
  pollution_gen_server:start_link(),
  pollution_gen_server:addStation("Poznan", {10, 10}),
  pollution_gen_server:addStation("Krakow", {20, 20}),
  pollution_gen_server:addValue("Krakow", todaySubtractHour(0), "PM10", 80),
  pollution_gen_server:addValue("Krakow", todaySubtractHour(1), "PM10", 70),
  pollution_gen_server:addValue("Krakow", todaySubtractHour(2), "PM10", 60),
  pollution_gen_server:addValue("Krakow", todaySubtractHour(0), "PM2.5", 50),
  pollution_gen_server:addValue("Poznan", todaySubtractHour(2), "PM10", 60),
  pollution_gen_server:addValue("Poznan", todaySubtractHour(0), "PM2.5", 30),
  pollution_gen_server:addValue("Poznan", todaySubtractHour(1), "PM2.5", 40).


getOneValue_test() ->
  prepareServer(),
  ?assertEqual(70, pollution_gen_server:getOneValue({20, 20}, todaySubtractHour(1), "PM10")),
  ?assertEqual(40, pollution_gen_server:getOneValue("Poznan", todaySubtractHour(1), "PM2.5")),
  pollution_gen_server:close().

removeValue_test() ->
  prepareServer(),
  ?assertEqual(ok, pollution_gen_server:removeValue("Krakow", todaySubtractHour(0), "PM10")),
  ?assertEqual(ok, pollution_gen_server:addValue("Krakow", todaySubtractHour(0), "PM10", 100)),
  ?assertEqual(100, pollution_gen_server:getOneValue("Krakow", todaySubtractHour(0), "PM10")),
  pollution_gen_server:close().

getStationMean_test() ->
  prepareServer(),
  ?assertEqual(70.0, pollution_gen_server:getStationMean("Krakow", "PM10")),
  ?assertEqual(35.0, pollution_gen_server:getStationMean({10,10}, "PM2.5")),
  ?assertMatch({error, _}, pollution_gen_server:getStationMean({10,10}, "Temperature")),
  pollution_gen_server:close().

getDailyMean_test() ->
  prepareServer(),
  {Date, _} = calendar:local_time(),
  ?assertEqual(40.0, pollution_gen_server:getDailyMean(Date, "PM2.5")),
  ?assertEqual(67.5, pollution_gen_server:getDailyMean(Date, "PM10")),
  {{Ye, Mo, Da}, _} = calendar:local_time(),
  Tomorrow = {Ye, Mo, Da+1},
  ?assertMatch({error, _}, pollution_gen_server:getDailyMean(Tomorrow, "PM10")),
  pollution_gen_server:close().

getMovingMean_test() ->
  prepareServer(),
  ?assertEqual(true, pollution_gen_server:getMovingMean({10,10}, "PM2.5") < 35.0),
  pollution_gen_server:close().

getNearestStation_test() ->
  prepareServer(),
  ?assertEqual({20, 20}, pollution_gen_server:getNearestStation({20, 30})),
  ?assertEqual({10, 10}, pollution_gen_server:getNearestStation({5, 5})),
  pollution_gen_server:addStation("Gdansk", {5, 10}),
  ?assertEqual({5, 10}, pollution_gen_server:getNearestStation({5, 5})),
  pollution_gen_server:close().

getNearestStationIndex_test() ->
  prepareServer(),
  ?assertEqual(1.0, pollution_gen_server:getNearestStationIndex({10, 12})),
  ?assertEqual(1.6666666666666667, pollution_gen_server:getNearestStationIndex({22, 18})),
  pollution_gen_server:close().

close_test() ->
  prepareServer(),
  ?assert(lists:member(pollution_gen_server, registered())),
  ?assertEqual(70, pollution_gen_server:getOneValue({20, 20}, todaySubtractHour(1), "PM10")),
  pollution_gen_server:close(),
  ?assert(not lists:member(pollution_gen_server, registered())).

