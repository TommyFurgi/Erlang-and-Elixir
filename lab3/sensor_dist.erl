%%%-------------------------------------------------------------------
%%% @author tomci
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Apr 2024 2:00 PM
%%%-------------------------------------------------------------------
-module(sensor_dist).
-author("tomci").

%% API
-export([get_rand_locations/1, dist/2, find_for_person/2, find_closest/2, find_for_person/3, find_closest_parallel/2]).


get_rand_locations(Number) ->
  [{rand:uniform(10000), rand:uniform(10000)} || _ <- lists:seq(1, Number)].


dist({X1, Y1}, {X2, Y2}) ->
  math:sqrt(math:pow(X1-X2, 2) + math:pow(Y1-Y2, 2)).

find_for_person(PersonLocation, SensorsLocations) ->
  lists:min([{dist(PersonLocation, Sensor), {PersonLocation, Sensor}} || Sensor <- SensorsLocations]).

find_closest(PeopleLocations, SensorsLocations) ->
  lists:min([find_for_person(P, SensorsLocations) || P <- PeopleLocations]).


find_for_person(PersonLocation, SensorsLocations, ParentPID) ->
  ParentPID ! find_for_person(PersonLocation, SensorsLocations).


find_closest_parallel(PeopleLocations, SensorsLocations) ->
  [spawn(?MODULE, find_for_person, [PersonLocation, SensorsLocations, self()]) || PersonLocation <- PeopleLocations],
  Data = [receive M -> M end || _ <- PeopleLocations],
  lists:min(Data).

%%timer:tc(sensor_dist, find_closest,[P, S]).
%%{8153700,{2.0,{{1028,477},{1026,477}}}}
%%timer:tc(sensor_dist, find_closest_parallel,[P, S]).
%%{1711206,{2.0,{{1028,477},{1026,477}}}}