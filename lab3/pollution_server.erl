%%%-------------------------------------------------------------------
%%% @author tomci
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Apr 2024 4:22 PM
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("tomci").

%% API
-export([start/0, init/0, stop/0, add_station/2, add_value/4, remove_value/3,
  get_one_value/3, get_station_mean/2, get_daily_mean/2,
  get_maximum_gradient_stations/0, get_monitor/0]).

start() ->
  register(server, spawn(?MODULE, init, [])).

stop() ->
  server ! stop.


init() ->
  pollution_loop(pollution:create_monitor()).


show_error(Message) ->
  io:format("ERROR: ~w. ~n", [Message]).


add_station(Name, Coordinates) ->
  server ! {add_station, {Name, Coordinates}}.

add_value(Id, Date, Type, Value) ->
  server ! {add_value, {Id, Date, Type, Value}}.

remove_value(Id, Date_time, Reading_type) ->
  server ! {remove_value, {Id, Date_time, Reading_type}}.

get_one_value(Station_id, Date_time, Reading_type) ->
  server ! {get_one_value, {Station_id, Date_time, Reading_type}, self()},
  receive M -> M end.

get_station_mean(Station_id, Reading_type) ->
  server ! {get_station_mean, {Station_id, Reading_type}, self()},
  receive M -> M end.

get_daily_mean(Reading_type, Date_time) ->
  server ! {get_daily_mean, {Reading_type, Date_time}, self()},
  receive M -> M end.

get_maximum_gradient_stations() ->
  server ! {get_maximum_gradient_stations, self()},
  receive M -> M end.

get_monitor() ->
  server ! {get_monitor, self()},
  receive M -> M end.

pollution_loop(Monitor) ->
  receive
    stop -> ok;

    {get_monitor, Pid} ->
      Pid ! Monitor,
      pollution_loop(Monitor);

    {add_station, {Name, Coordinates}} ->
      New_monitor = pollution:add_station(Name, Coordinates, Monitor),
      case New_monitor of
        {error, Message} ->
          show_error(Message),
          pollution_loop(Monitor);
        _ ->
          pollution_loop(New_monitor)
      end;

    {add_value, {Id, Date, Type, Value}} ->
      New_monitor = pollution:add_value(Id, Date, Type, Value, Monitor),
      case New_monitor of
        {error, Message} ->
          show_error(Message),
          pollution_loop(Monitor);
        _ ->
          pollution_loop(New_monitor)
      end;

    {remove_value, {Id, Date_time, Reading_type}} ->
      New_monitor = pollution:remove_value(Id, Date_time, Reading_type, Monitor),
      case New_monitor of
        {error, Message} ->
          show_error(Message),
          pollution_loop(Monitor);
        _ ->
          pollution_loop(New_monitor)
      end;

    {get_one_value, {Station_id, Date_time, Reading_type}, Pid} ->
      Value = pollution:get_one_value(Station_id, Date_time, Reading_type, Monitor),
      case Value of
        {error, Message} -> show_error(Message);
        _ -> ok
      end,
      Pid ! Value,
      pollution_loop(Monitor);

    {get_station_mean, {Station_id, Reading_type}, Pid} ->
      Value = pollution:get_station_mean(Station_id, Reading_type, Monitor),
      case Value of
        {error, Message} -> show_error(Message);
        _ -> ok
      end,
      Pid ! Value,
      pollution_loop(Monitor);

    {get_daily_mean, {Reading_type, Date_time}, Pid} ->
      Value = pollution:get_daily_mean(Reading_type, Date_time, Monitor),
      case Value of
        {error, Message} -> show_error(Message);
        _ -> ok
      end,
      Pid ! Value,
      pollution_loop(Monitor);

    {get_maximum_gradient_stations, Pid} ->
      Value = pollution:get_maximum_gradient_stations(Monitor),
      case Value of
        {error, Message} -> show_error(Message);
        _ -> ok
      end,
      Pid ! Value,
      pollution_loop(Monitor);

    Command ->
      io:format("ERROR: Unknown server command: ~w. ~n", [Command]),
      pollution_loop(Monitor)

  end.

