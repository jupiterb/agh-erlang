%%%-------------------------------------------------------------------
%%% @author Piotr Biały
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. maj 2021 21:04
%%%-------------------------------------------------------------------
-module(simulation).
-author("Piotr Biały").

%% API
-export([create_simulation/4, simulation/4]).

% Simulation creator

create_simulation(Size, Population, InfectedProbability, Epochs) ->
  io:format("Creating Simulation...~n"),
  Humans = create_humans(Population, InfectedProbability, []),
  Stats = create_stats(),
  build_humans(create_world(Size), Stats, create_settings(), Humans),
  io:format("Simulation pid: ~p~n", [self()]),
  simulation(Humans, Stats, Epochs, Population).

% create helpers (atomics and counters)

create_stats() ->
  counters:new(7, [write_concurrency]).

create_settings() ->
  DaysInState = atomics:new(6, [{signed, true}]),
  atomics:put(DaysInState, 1, 5),    % min days in S state
  atomics:put(DaysInState, 2, 7),   % min days in E state
  atomics:put(DaysInState, 3, 2),   % min days in I state
  atomics:put(DaysInState, 4, 45),   % min days in R state
  atomics:put(DaysInState, 5, 7),   % min days in H state
  atomics:put(DaysInState, 6, 1),    % min days in D state
  ProbabilitiesOfStates = atomics:new(6, [{signed, true}]), % in percents
  atomics:put(ProbabilitiesOfStates, 1, 10),  % probability of start of state S
  atomics:put(ProbabilitiesOfStates, 2, 50),  % probability of start of state E
  atomics:put(ProbabilitiesOfStates, 3, 50),  % probability of start of state I
  atomics:put(ProbabilitiesOfStates, 4, 65), % probability of start of state R
  atomics:put(ProbabilitiesOfStates, 5, 65), % probability of start of state H
  atomics:put(ProbabilitiesOfStates, 6, 11), % probability of start of state D
  {DaysInState, ProbabilitiesOfStates}.

create_world(Size) ->
  counters:new(Size*Size, [write_concurrency]).

% create helpers (humans)

create_humans(0, _, Humans) ->
  Humans;

create_humans(HumansCount, InfectedProbability, Humans) ->
  case rand:uniform() > InfectedProbability of
    true -> create_humans(HumansCount - 1, InfectedProbability,
      [spawn(human, create, [1]) | Humans]);
    false -> create_humans(HumansCount - 1, InfectedProbability,
      [spawn(human, create, [2]) | Humans])
  end.

build_humans(_, _, _, []) ->
  ok;

build_humans(World, Stats, Settings, [Human | RestOfPopulation]) ->
  erlang:send(Human, {living_at, World}),
  erlang:send(Human, {with_pandemic, Settings}),
  erlang:send(Human, {counted_in, Stats}),
  erlang:send(Human, complete),
  build_humans(World, Stats, Settings, RestOfPopulation).

% simulation

simulation(Humans, _, 0, _) ->
  stop_humans(Humans);

simulation(Humans, Stats, Epoch, Population) ->
  receive
    stop -> stop_humans(Humans)
  after 100 ->
    print_stats(Stats, Population),
    simulation(Humans, Stats, Epoch-1, Population)
  end.

print_stats(Stats, Population) ->
  io:format("State S: ~w ~n", [counters:get(Stats, 1)]),
  io:format("State E: ~w ~n", [counters:get(Stats, 2)]),
  io:format("State I: ~w ~n", [counters:get(Stats, 3)]),
  io:format("State R: ~w ~n", [counters:get(Stats, 4)]),
  io:format("State H: ~w ~n", [counters:get(Stats, 5)]),
  io:format("State D: ~w ~n", [counters:get(Stats, 6)]),
  io:format("Days of panedmic: ~f ~n", [(counters:get(Stats, 7))/ Population]),
  io:format("**********************~n").

stop_humans([]) ->
  ok;

stop_humans([Human | RestOfPopulation]) ->
  erlang:send(Human, stop),
  stop_humans(RestOfPopulation).
