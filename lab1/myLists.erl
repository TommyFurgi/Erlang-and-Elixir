-module(myLists).
-author("tomci").

-export([containts/2, duplicateElements/1, sumFloats/1, sumFloats2/2, number_of_readings/1, calculate_max/2, get_type_value/2, calculate_mean/2]).

containts([],_) -> false;
containts([H|_], A) when H == A -> true;
containts([_|T], A) -> containts(T, A).

duplicateElements([]) -> [];
duplicateElements([H|T]) -> [H,H] ++ duplicateElements(T).


sumFloats2([], Suma) -> Suma;
sumFloats2([H|T], Suma) -> sumFloats2(T, Suma+H).

sumFloats([]) -> 0.0;
sumFloats(L) -> sumFloats2(L, 0.0).


%%ZADANIE
%%1.
number_of_readings(Data) when is_list(Data) -> number_of_readings(Data, 0);
number_of_readings(_) ->
  io:format("Error in number_of_readings/1!"),
  {error, cannot_calculate}.

number_of_readings([], Count) -> Count;
number_of_readings([_|T], Count) -> number_of_readings(T, Count + 1).


%%2.
is_string(String) ->
  lists:all(fun (Char) -> is_integer(Char) end, String).

calculate_max(Readings, Type) when is_list(Readings) ->
  case is_string(Type) of
    true ->
      calculate_max(Readings, Type, 0.0);
    false ->
      {error, not_a_string}
  end;
calculate_max(_,_) ->
  {error, not_a_list}.

calculate_max([], _, Max) -> Max;
calculate_max([H|T], Type, Max) ->
  {_,_,Measurements} = H,
  Value = get_type_value(Measurements, Type),
  if
    Value > Max ->
      calculate_max(T, Type, Value);
    true ->
      calculate_max(T, Type, Max)
  end.

get_type_value([],_) -> -1.0;
get_type_value([{Type_pom, Val}|T], Type) ->
  if
    Type_pom == Type ->
      Val;
    true ->
      get_type_value(T, Type)
  end.


%%3.
calculate_mean(Readings, Type) when is_list(Readings) ->
  case is_string(Type) of
    true ->
      calculate_mean(Readings, Type, 0.0, 0);
    false ->
      {error, not_a_string}
  end;
calculate_mean(_,_) ->
  {error, not_a_list}.

calculate_mean([], _, Sum, Count) ->
  if
    Sum == 0.0 -> 0.0;
    true -> Sum/Count
  end;

calculate_mean([H|T], Type, Sum, Count) ->
  {_,_,Measurements} = H,
  Value = get_type_value(Measurements, Type),
  if
    Value =/= -1.0 ->
      calculate_mean(T, Type, Sum+Value, Count+1);
    true ->
      calculate_mean(T, Type, Sum, Count)
  end.

