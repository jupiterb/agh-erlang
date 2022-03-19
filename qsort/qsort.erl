%%%-------------------------------------------------------------------
%%% @author Piotr Biały
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. mar 2021 11:31
%%%-------------------------------------------------------------------
-module(qsort).
-author("Piotr Biały").

%% API
-export([qs/1, randomElems/3, compareSpeeds/3]).


lessThan(List, Arg) ->
  [X || X <- List, X < Arg].

grtEqThan(List, Arg) ->
  [X || X <- List, X >= Arg].


qs([]) -> [];

qs([Pivot|Tail]) ->
  qs( lessThan(Tail, Pivot) ) ++ [Pivot] ++ qs( grtEqThan(Tail, Pivot)).


randomElems(N,Min,Max)->
  [rand:uniform(Max + 1 - Min) + Min - 1 || _ <- lists:seq(1, N)].

compareSpeeds(List, Fun1, Fun2) ->
  {Time1, _} = timer:tc(Fun1, [List]),
  {Time2, _} = timer:tc(Fun2, [List]),
  io:format("Time of execution: Fun1: ~w, Fun2: ~w~n", [Time1, Time2]).
