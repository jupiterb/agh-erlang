%%%-------------------------------------------------------------------
%%% @author Piotr Biały
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. maj 2021 14:29
%%%-------------------------------------------------------------------
-module(pollution_supervisor).
-author("Piotr Biały").

-behaviour(supervisor).
%% API
-export([start_link/0, init/1]).


start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  RestartFlags = #{strategy => one_for_one,
                  intensity => 2,
                  period => 3},

  ChildSpec = #{id => 'pollution_server',
                start => {
                  pollution_gen_server,
                  start_link,
                  []
                },
                restart => transient,
                shutdown => 100000,
                type => worker,
                modules => [pollution_gen_server]},

  {ok, {RestartFlags, [ChildSpec]}}.
