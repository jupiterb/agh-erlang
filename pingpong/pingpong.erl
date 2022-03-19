%%%-------------------------------------------------------------------
%%% @author Piotr Biały
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. kwi 2021 11:26
%%%-------------------------------------------------------------------
-module(pingpong).
-author("Piotr Biały").

%% API
-export([
  start/0,
  stop/0,
  play/1,
  ping/1,
  pong/1
]).


start() ->
  register(ping, spawn(pingpong, ping, [0])),
  register(pong, spawn(pingpong, pong, [0])).

stop() ->
  ping ! stop,
  pong ! stop.

play(N) ->
  ping ! {start, N}.

ping(Sum) ->
  receive
    stop -> ok;
    {start, N} ->
      pong ! {msg, N},
      ping(Sum);
    {msg, 0} -> ping(Sum);
    {msg, N} ->
      pong ! {msg, N-1},
      io:format("Ping ~w~n", [Sum]),
      timer:sleep(100),
      ping(Sum + 1)
  after 20000 ->
    io:format("finito~n")
  end.

pong(Sum) ->
  receive
    stop -> ok;
    {msg, 0} -> pong(Sum);
    {msg, N} ->
      ping ! {msg, N},
      io:format("Pong ~w~n", [Sum]),
      timer:sleep(100),
      pong(Sum + 1)
  after 20000 ->
    io:format("finito~n")
  end.