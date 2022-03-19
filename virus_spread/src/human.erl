%%%-------------------------------------------------------------------
%%% @author Piotr Biały
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. maj 2021 21:05
%%%-------------------------------------------------------------------
-module(human).
-author("Piotr Biały").

%% API
-export([create/1, build/1, update/1]).

-record(pandemic_status, {state, days, virus}).
-record(health, {innateImmunity, acquiredImmunity, alternativeMedicine}).
-record(arrays, {world, days_in_state, proba_of_state, stats}).
-record(human, {pandemic_status, health, location, arrays}).

create(State) ->
  PandemicStatus = #pandemic_status
  {
    state = State,
    days = 0,
    virus = rules:get_virus()
  },
  Health = #health
  {
    innateImmunity = rand:normal(0.0, 0.1),
    acquiredImmunity = rand:normal(0.1, 0.15),
    alternativeMedicine = rand:normal(0.2, 0.1)
  },
  Arrays = #arrays
  {
    world = null,
    days_in_state = null,
    proba_of_state = null,
    stats = null
  },
  Human = #human
  {
    pandemic_status = PandemicStatus,
    health = Health,
    location = null,
    arrays = Arrays
  },
  build(Human).

build(#human{pandemic_status = #pandemic_status{state = State} = PandemicStatus, arrays = Arrays} = Human) ->

  receive
    {living_at, World} ->
      build(#human
      {
        pandemic_status = PandemicStatus,
        health = Human#human.health,
        location = random_location(World),
        arrays = #arrays
        {
          world = World,
          days_in_state = Arrays#arrays.days_in_state,
          proba_of_state = Arrays#arrays.proba_of_state,
          stats = Arrays#arrays.stats
        }
      });

    {with_pandemic, {DaysInState, ProbaOfState} = _Settings} ->
      build(#human
      {
        pandemic_status = PandemicStatus,
        health = Human#human.health,
        location = Human#human.location,
        arrays = #arrays
        {
          world = Arrays#arrays.world,
          days_in_state = DaysInState,
          proba_of_state = ProbaOfState,
          stats = Arrays#arrays.stats
        }
      });

    {counted_in, Stats} ->
      counters:add(Stats, State, 1),
      build(#human
      {
        pandemic_status = PandemicStatus,
        health = Human#human.health,
        location = Human#human.location,
        arrays = #arrays
        {
          world = Arrays#arrays.world,
          days_in_state = Arrays#arrays.days_in_state,
          proba_of_state = Arrays#arrays.proba_of_state,
          stats = Stats
        }
      });

    complete -> update(Human)
  end.

random_location(World) ->
  #{size := Size} = counters:info(World),
  Limit = round(math:sqrt(Size)),
  {rand:uniform(Limit), rand:uniform(Limit), Limit}.


update(#human{location = Location, pandemic_status = #pandemic_status{state = State}} = Human) ->
  receive
    stop -> ok
  after 10 ->
    counters:add(Human#human.arrays#arrays.stats, 7, 1),
    case State =:= 6 of
      true -> update(Human);
      false ->
        OldState = Human#human.pandemic_status#pandemic_status.state,
        PandemicStatus = update_state(Human),
        NewState = PandemicStatus#pandemic_status.state,
        update_stats(OldState, NewState, Human#human.arrays#arrays.stats),
        update(#human{
          pandemic_status = PandemicStatus,
          location = update_location(Location),
          health = Human#human.health,
          arrays = Human#human.arrays
        })
    end
  end.

update_stats(OldState, NewState, Stats) ->
  case OldState =:= NewState of
    true -> ok;
    false ->
      counters:sub(Stats, OldState, 1),
      counters:add(Stats, NewState, 1),
      ok
  end.

update_location({X, Y, Limit}) ->
  XMove = rand:uniform(3) - 2,
  YMove = rand:uniform(3) - 2,
  case {X + XMove > Limit orelse X + XMove < 1, Y + YMove > Limit orelse Y + YMove < 1} of
    {true, true} ->   {1, 1, Limit};
    {true, false} ->  {1, Y + YMove, Limit};
    {false, true} ->  {X + XMove, 1, Limit};
    {false, false} -> {X + XMove, Y + YMove, Limit}
  end.

update_state(#human{pandemic_status = #pandemic_status{state = State, days = Days, virus = Virus} = PandemicStatus} = Human)
  when State =:= 1->

  case atomics:get(Human#human.arrays#arrays.days_in_state, State) =< Days of
    false -> #pandemic_status{state = State, days = Days + 1, virus = Virus};
    true ->
      Proba = rules:trans_S_E(
        atomics:get(Human#human.arrays#arrays.proba_of_state, 2) / 100,
        Human#human.health#health.innateImmunity,
        PandemicStatus#pandemic_status.virus,
        Human#human.location,
        Human#human.arrays#arrays.world),
      rules:get_state(Proba, Proba, 2, 2, PandemicStatus, Human#human.arrays#arrays.world, Human#human.location)
  end;

update_state(#human{pandemic_status = #pandemic_status{state = State, days = Days, virus = Virus} = PandemicStatus} = Human)
  when State =:= 2->

  case atomics:get(Human#human.arrays#arrays.days_in_state, State) =< Days of
    false -> #pandemic_status{state = State, days = Days + 1, virus = Virus};
    true ->
      Proba1 = rules:trans_E_I(
        atomics:get(Human#human.arrays#arrays.proba_of_state, 4) / 100,
        Human#human.health#health.innateImmunity,
        PandemicStatus#pandemic_status.virus),
      Proba2 = rules:trans_E_R(
        atomics:get(Human#human.arrays#arrays.proba_of_state, 4) / 100,
        Human#human.health#health.innateImmunity,
        PandemicStatus#pandemic_status.virus),
      rules:get_state(Proba1, Proba2, 3, 4, PandemicStatus, Human#human.arrays#arrays.world, Human#human.location)
  end;

update_state(#human{pandemic_status = #pandemic_status{state = State, days = Days, virus = Virus} = PandemicStatus} = Human)
  when State =:= 3->

  case atomics:get(Human#human.arrays#arrays.days_in_state, State) =< Days of
    false -> #pandemic_status{state = State, days = Days + 1, virus = Virus};
    true ->
      Proba1 = rules:trans_I_H(
        atomics:get(Human#human.arrays#arrays.proba_of_state, 5) / 100,
        Human#human.health#health.alternativeMedicine),
      Proba2 = rules:trans_I_R(
        atomics:get(Human#human.arrays#arrays.proba_of_state, 4) / 100,
        Human#human.health#health.innateImmunity,
        PandemicStatus#pandemic_status.virus),
      rules:get_state(Proba1, Proba2, 5, 4, PandemicStatus, Human#human.arrays#arrays.world, Human#human.location)
  end;

update_state(#human{pandemic_status = #pandemic_status{state = State, days = Days, virus = Virus} = PandemicStatus} = Human)
  when State =:= 4->

  case atomics:get(Human#human.arrays#arrays.days_in_state, State) =< Days of
    false -> #pandemic_status{state = State, days = Days + 1, virus = Virus};
    true ->
      Proba1 = rules:trans_R_S(
        atomics:get(Human#human.arrays#arrays.proba_of_state, 1) / 100,
        Human#human.health#health.acquiredImmunity),
      Proba2 = rules:trans_R_E(
        atomics:get(Human#human.arrays#arrays.proba_of_state, 2) / 100,
        Human#human.health#health.innateImmunity,
        Human#human.health#health.acquiredImmunity,
        PandemicStatus#pandemic_status.virus,
        Human#human.location,
        Human#human.arrays#arrays.world),
      rules:get_state(Proba1, Proba2, 1, 2, PandemicStatus, Human#human.arrays#arrays.world, Human#human.location)
  end;

update_state(#human{pandemic_status = #pandemic_status{state = State, days = Days, virus = Virus} = PandemicStatus} = Human)
  when State =:= 5->

  case atomics:get(Human#human.arrays#arrays.days_in_state, State) =< Days of
    false -> #pandemic_status{state = State, days = Days + 1, virus = Virus};
    true ->
      Proba1 = rules:trans_H_D(
        atomics:get(Human#human.arrays#arrays.proba_of_state, 6) / 100,
        PandemicStatus#pandemic_status.virus),
      Proba2 = rules:trans_H_R(
        atomics:get(Human#human.arrays#arrays.proba_of_state, 4) / 100,
        PandemicStatus#pandemic_status.virus),
      rules:get_state(Proba1, Proba2, 6, 4, PandemicStatus, Human#human.arrays#arrays.world, Human#human.location)
  end;

update_state(#human{pandemic_status = #pandemic_status{state = State} = PandemicStatus}) when State =:= 6 ->
  PandemicStatus.
