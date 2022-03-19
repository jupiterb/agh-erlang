%%%-------------------------------------------------------------------
%%% @author Piotr Biały
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. mar 2021 15:07
%%%-------------------------------------------------------------------
-module(onp).
-author("Piotr Biały").

%% API
-export([onp/1]).


% [H | T] - expression ; [A,B | S] - stack.

onp(["+"|T], [A,B | S]) ->
  onp(T, [B+A | S]);

onp(["-"|T], [A,B | S]) ->
  onp(T, [B-A | S]);

onp(["*"|T], [A,B | S]) ->
  onp(T, [B*A | S]);

onp(["/"|T], [A,B | S]) ->
  onp(T, [B/A | S]);

onp(["pow"|T], [A,B | S]) ->
  onp(T, [math:pow(B,A) | S]);

onp(["sqrt"|T], [A | S]) ->
  onp(T, [math:sqrt(A) | S]);

onp(["sin"|T], [A | S]) ->
  onp(T, [math:sin(A) | S]);

onp(["cos"|T], [A | S]) ->
  onp(T, [math:cos(A) | S]);

onp(["tan"|T], [A | S]) ->
  onp(T, [math:tan(A) | S]);

onp(["ctg"|T], [A | S]) ->
  onp(T, [1/math:tan(A) | S]);

onp([H|T], [A,B | S]) ->
  case string:to_float(H) of
    {error,_} ->
      case string:to_integer(H) of
        {error, _} -> "Ivalid expression or somthing like that";
        {Int, _} -> onp(T, [Int | [A,B | S]])
      end;
    {Float, _} -> onp(T, [Float | [A,B | S]])
  end;

onp([], [A | S]) ->
  A.


onp(Expression) ->
  onp(string:tokens(Expression, " "), [0,0]).
