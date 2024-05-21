 %%%-------------------------------------------------------------------
%%% @author tomci
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Apr 2024 5:09 PM
%%%-------------------------------------------------------------------
-module(pollution_gen_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, crash/0]).

-export([add_station/2, add_value/4, remove_value/3]).
-export([get_monitor/0, get_one_value/3, get_daily_mean/2, get_station_mean/2, get_maximum_gradient_stations/0]).

-define(SERVER, ?MODULE).

-record(pollution_gen_server_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
 gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
 {ok, pollution:create_monitor()}.

crash() -> 1/0.


%%handle_call(add_station, Monitor) ->
%%%%  {noreply, pollution:add_station(Name, Coo, Monitor)}.
%%  {noreply, State}.

add_station(Name, Coordinates) ->
 gen_server:cast(?MODULE, {add_station, Name, Coordinates}).

add_value(Id, Date, Type, Value) ->
 gen_server:cast(?MODULE, {add_value, Id, Date, Type, Value}).

remove_value(Id, Date_time, Reading_type) ->
 gen_server:cast(?MODULE, {remove_value, Id, Date_time, Reading_type}).

get_one_value(Station_id, Date_time, Reading_type) ->
 gen_server:call(?MODULE, {get_one_value, Station_id, Date_time, Reading_type}).


get_station_mean(Station_id, Reading_type) ->
 gen_server:call(?MODULE, {get_station_mean, Station_id, Reading_type}).

get_daily_mean(Reading_type, Date_time) ->
 gen_server:call(?MODULE, {get_daily_mean, Reading_type, Date_time}).

get_maximum_gradient_stations() ->
 gen_server:call(?MODULE, get_maximum_gradient_stations).

get_monitor() ->
 gen_server:call(?MODULE, get_monitor).


handle_cast(Cast_arguments, Current_monitor) ->
 Result = case Cast_arguments of
            {add_station, Name, Coordinates} ->
              pollution:add_station(Name, Coordinates, Current_monitor);

            {add_value, Station_id, Date_time, Reading_type, Reading_value} ->
              pollution:add_value(Station_id, Date_time, Reading_type, Reading_value, Current_monitor);

            {remove_value, Station_id, Reading_date_time, Reading_type} ->
              pollution:remove_value(Station_id, Reading_date_time, Reading_type, Current_monitor);

            _ ->
              {error, unknown_cast_argument}
          end,

 case Result of
   {error, Error_message} ->
%%      {stop, Error_message, Current_monitor};
     show_error(Error_message),
     {noreply, Current_monitor};
   _ ->
     {noreply, Result}
 end.

handle_call(Call_arguments, _From, Current_monitor) ->
  Result = case Call_arguments of
            get_monitor ->
              Current_monitor;

            {get_one_value, Station_id, Reading_date_time, Reading_type} ->
              pollution:get_one_value(Station_id, Reading_date_time, Reading_type, Current_monitor);

            {get_station_mean, Station_id, Reading_type} ->
              pollution:get_station_mean(Station_id, Reading_type, Current_monitor);

            {get_daily_mean, Reading_type, Date} ->
              pollution:get_daily_mean(Reading_type, Date, Current_monitor);

            get_maximum_gradient_stations ->
              pollution:get_maximum_gradient_stations(Current_monitor)
          end,

 case Result of
   {error, Error_message} ->
%%      {stop, Error_message, Current_monitor};
     show_error(Error_message),
     {noreply, Current_monitor};
   _ ->
     {reply, Result, Current_monitor}
 end.

show_error(Message) ->
 io:format("ERROR: ~w. ~n", [Message]).

terminate(Reason, State) ->
 io:format("ERROR: ~w. ~n", [Reason]),
 io:format("Last state was: ~w. ~n", [State]),
 ok.


code_change(_OldVsn, State = #pollution_gen_server_state{}, _Extra) ->
 {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
