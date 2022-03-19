%%%-------------------------------------------------------------------
%%% @author Piotr Biały
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. maj 2021 14:04
%%%-------------------------------------------------------------------
-module(pollution_app).
-author("Piotr Biały").

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  pollution_supervisor:start_link().

stop(_State) ->
  ok.
