%%%-------------------------------------------------------------------
%%% @author Piotr Biały
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. maj 2021 19:09
%%%-------------------------------------------------------------------
-module(rules).
-author("Piotr Biały").

%% API
-export([get_virus/0,
        get_state/7,
        trans_S_E/5,
        trans_R_E/6,
        trans_E_R/3,
        trans_R_S/2,
        trans_E_I/3,
        trans_I_H/2,
        trans_I_R/3,
        trans_H_D/2,
        trans_H_R/2]).
-record(pandemic_status, {state, days, virus}).


get_virus() -> {rand:normal(0.0, 0.2), rand:normal(0.1, 0.1)}. % {MortalityBias, Contagiousness}

get_state(Proba1, Proba2, State1, State2, #pandemic_status{state = State, days = Days, virus = Virus}, World, Location) ->
  Change = Proba1 / 2 + Proba2 / 2,
  Random = rand:uniform(),
  case Random =< Change of
    true ->
      case Random < Proba1 / 2 of
        true ->
          new_sick(World, Location, State1),
          new_health(World, Location, State, State1),
          #pandemic_status{state = State1, days = 0, virus = new_virus(State1, Virus)};
        false ->
          new_sick(World, Location, State2),
          new_health(World, Location, State, State2),
          #pandemic_status{state = State2, days = 0, virus = new_virus(State2, Virus) }
      end;
    false -> #pandemic_status{state = State, days = Days + 1, virus = Virus}
  end.

location_to_index(Location) ->
  {X, Y, Limit} = Location,
  (X-1) * Limit + Y.

new_sick(World, Location, NewState) ->
  Index = location_to_index(Location),
  case NewState of
    2 -> counters:add(World, Index, 1);
    _ -> ok
  end.

new_health(World, Location, OldStatus, NewState) ->
  Index = location_to_index(Location),
  case OldStatus =:= 3 orelse (OldStatus =:= 2 andalso NewState =:= 4) of
    true -> counters:sub(World, Index, 1);
    false -> ok
  end.

new_virus(2, _) -> get_virus();
new_virus(_, Virus) -> Virus.


scale(InfectedCount) ->
  min(InfectedCount / 7, 1.0).

% healthy person without antibodies -> asymptomatic sick man
trans_S_E(ProbabilityE, InnateImmunityBias, {_, ContagiousnessBias}, Location, World) ->
  {X, Y, Limit} = Location,
  Scale = scale(counters:get(World, (X-1)*Limit + Y)),
  min(max(ProbabilityE * Scale + ContagiousnessBias - InnateImmunityBias, 0.0), 1.0).

% healthy person with antibodies -> asymptomatic sick man
trans_R_E(ProbabilityE, InnateImmunityBias, AcquiredImmunityBias, {_, ContagiousnessBias}, Location, World) ->
  {X, Y, Limit} = Location,
  Scale = scale(counters:get(World, (X-1)*Limit + Y)),
  min(max(ProbabilityE * Scale + ContagiousnessBias - AcquiredImmunityBias - InnateImmunityBias, 0.0), 1.0).

% asymptomatic sick man -> healthy person with antibodies
trans_E_R(ProbabilityR, InnateImmunityBias, {MortalityBias, _}) ->
  min(max(ProbabilityR + InnateImmunityBias - MortalityBias, 0.0), 1.0).

% healthy person with antibodies -> healthy person without antibodies
trans_R_S(ProbabilityS, AcquiredImmunityBias) ->
  min(ProbabilityS - AcquiredImmunityBias, 1.0).


% asymptomatic sick man -> sick person with symptoms, untested
trans_E_I(ProbabilityR, InnateImmunityBias, Virus) ->
  1 - trans_E_R(ProbabilityR, InnateImmunityBias, Virus).

% sick person with symptoms, untested -> sick person with symptoms, hospitalized
trans_I_H(ProbabilityH, AlternativeMedicineBias) ->
  min(ProbabilityH + AlternativeMedicineBias, 1.0).

% sick person with symptoms, untested -> healthy person with antibodies
trans_I_R(ProbabilityR, InnateImmunityBias, Virus) ->
  1 - trans_E_R(ProbabilityR, InnateImmunityBias, Virus).


% sick person with symptoms, hospitalized -> died
trans_H_D(ProbabilityD, {MortalityBias, _}) ->
  min(ProbabilityD + MortalityBias, 1.0).

% sick person with symptoms, hospitalized -> healthy person with antibodies
trans_H_R(ProbabilityD, Virus) ->
  1 - trans_H_D(ProbabilityD, Virus).