%%%-------------------------------------------------------------------
%%% @author tomci
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Mar 2024 5:36 PM
%%%-------------------------------------------------------------------
-module(pollution).
-author("tomci").

-export([create_monitor/0, add_station/3, add_value/5, remove_value/4, get_one_value/4, get_station_mean/3, get_daily_mean/3, get_station/2, get_maximum_gradient_stations/1]).

-record(monitor, {name_map, coordinate_map}).
-record(station, {name, coordinates, readings=[]}).
-record(reading, {type, value, date_time}).

get_station(Id, Monitor) ->
  Station = case Id of
              {_, _} ->
                case maps:is_key(Id, Monitor#monitor.coordinate_map) of
                  true -> maps:get(Id, Monitor#monitor.coordinate_map);
                  false -> {error, no_such_coordinates}
                end;
              _ ->
                case maps:is_key(Id, Monitor#monitor.name_map) of
                  true -> maps:get(Id, Monitor#monitor.name_map);
                  false -> {error, no_such_station_name}
                end
            end,
  Station.


update_station_readings(Station, Readings, Monitor) ->
  New_station = Station#station{readings = Readings},
  Station_name = New_station#station.name,

  {X, Y} = New_station#station.coordinates,
  Name_map = Monitor#monitor.name_map,
  Coordinate_map = Monitor#monitor.coordinate_map,
  Updated_name_map = maps:update(Station_name, New_station, Name_map),
  Updated_coordinate_map = maps:update({X, Y}, New_station, Coordinate_map),
  #monitor{name_map = Updated_name_map, coordinate_map = Updated_coordinate_map}.



create_monitor() -> #monitor{name_map = maps:new(), coordinate_map = maps:new()}.

add_station(Name, Coordinates, Monitor) ->
  Is_id_unique = not maps:is_key(Name, Monitor#monitor.name_map)
    and not maps:is_key(Coordinates, Monitor#monitor.coordinate_map),

  case Is_id_unique of
    false -> {error, station_is_not_unique};
    true -> Station = #station{name = Name, coordinates = Coordinates},
      #monitor{name_map = maps:put(Name, Station, Monitor#monitor.name_map),
        coordinate_map = maps:put(Coordinates, Station, Monitor#monitor.coordinate_map)}
    end.


add_value(Id, Date, Type, Value, Monitor) ->
  Reading = #reading{type = Type, value = Value, date_time = Date},
  Station = get_station(Id, Monitor),

  case Station of
    {error, _} ->
      {error, no_station_with_such_id};
      _ ->
        case get_one_value(Id, Date, Type, Monitor) of
          {error, _} -> update_station_readings(Station, [Reading | Station#station.readings], Monitor);
          _ -> {error, such_value_already_exists}
        end
  end.

remove_value(Id, Date_time, Reading_type, Monitor) ->
  Station = get_station(Id, Monitor),
  case Station of
    {error, _} -> {error, no_such_station};
    _ ->
        Filter = fun
                 (R) when R#reading.date_time == Date_time andalso R#reading.type == Reading_type -> false;
                 (_) -> true
                 end,

        Readings = lists:filter(Filter, Station#station.readings),
        case get_one_value(Id, Date_time, Reading_type, Monitor) of
          {error, _} -> {error, value_doesnt_exist};
          _ -> update_station_readings(Station, Readings, Monitor)
        end
  end.


get_one_value(Station_id, Date_time, Reading_type, Monitor) ->
  Station = get_station(Station_id, Monitor),
  case Station of
    {error, _} -> {error, no_such_station};
    _ ->
      Filter = fun
                 (R) when R#reading.date_time == Date_time andalso R#reading.type == Reading_type -> true;
                 (_) -> false
               end,


      Readings = lists:filter(Filter, Station#station.readings),


      case Readings of
        [] -> {error, no_readings_with_that_type_and_date};
        [H | _] -> H#reading.value
      end
  end.

get_station_mean(Station_id, Reading_type, Monitor) ->
  Station = get_station(Station_id, Monitor),
  case Station of
    {error, _} -> {error, no_such_station};
    _ ->
      Values = [R#reading.value || R <- Station#station.readings, R#reading.type == Reading_type],

      case Values of
        [] -> {error, no_data_for_this_station};
        _ -> lists:sum(Values) / length(Values)
      end
  end.

get_daily_mean(Reading_type, Date_time, Monitor) ->
  Stations_list = maps:values(Monitor#monitor.name_map),
  Values = get_values_from_stations(Stations_list, Reading_type, Date_time),

  case Values of
    [] -> {error, no_data_for_this_type};
    _ -> lists:sum(Values) / length(Values)
  end.



get_values_from_stations([], _, _) -> [];
get_values_from_stations([Station | Stations], Reading_type, Date_time) ->
  Readings = Station#station.readings,
  Values = [R#reading.value || R <- Readings, R#reading.type == Reading_type, reading_date_match(R#reading.date_time, Date_time)],
  Values ++ get_values_from_stations(Stations, Reading_type, Date_time).

reading_date_match({Reading_date,_}, Date_time) when Reading_date == Date_time -> true;
reading_date_match({_, _}, _) -> false.


% EXTRA TASK
distance({X1,Y1}, {X2, Y2}) ->
  math:sqrt(math:pow(X1-X2, 2) + math:pow(Y1-Y2, 2)).

get_maximum_gradient_stations(Monitor) ->
  StationPairs = generate_station_pairs(Monitor#monitor.name_map),
  case StationPairs of
    [] -> {error, not_enough_data};
    _ ->
      MaxGradient = lists:max([calculate_gradient(Station1, Station2) || {Station1, Station2} <- StationPairs]),
      [{Station1, Station2} || {Station1, Station2} <- StationPairs, calculate_gradient(Station1, Station2) == MaxGradient]
  end.

calculate_gradient(Station1, Station2) when Station1#station.readings /= [] andalso Station2#station.readings /= [] ->
  Distance = distance(Station1#station.coordinates, Station2#station.coordinates),
  MaxValue1 = lists:max([Value || #reading{value = Value} <- Station1#station.readings]),
  MaxValue2 = lists:max([Value || #reading{value = Value} <- Station2#station.readings]),
  abs(MaxValue2 - MaxValue1) / Distance;
calculate_gradient(_, _) -> 0.

generate_station_pairs(StationMap) ->
  StationList = maps:values(StationMap),
  StationPairs = generate_pairs(StationList),
  StationPairs.

generate_pairs([]) -> [];
generate_pairs([_]) -> [];
generate_pairs([H|T]) -> generate_pairs(H, T) ++ generate_pairs(T).

generate_pairs(_, []) -> [];
generate_pairs(Station, [NextStation|Rest]) ->
  [{Station, NextStation} | generate_pairs(Station, Rest)].

