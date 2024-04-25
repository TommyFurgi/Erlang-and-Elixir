%%%-------------------------------------------------------------------
%%% @author tomci
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Apr 2024 1:24 PM
%%%-------------------------------------------------------------------
-module(pingpong).
-author("tomci").

%% API
-export([start/0, stop/0, play/1, pong/0]).


start() ->
  register(ping, spawn(fun() -> ping(0, 0) end)),
  register(pong, spawn(?MODULE, pong, [])).

play(N) -> ping ! N.

ping(Sum, Loop_number) ->
  receive
    0 -> io:format("Ping otrzymal 0~n"), ping(Sum, Loop_number + 1);
    stop -> ok;
    N ->
      io:format("Ping otrzymal ~w; Current sum: ~w; Current loop number: ~w ~n", [N, Sum + N, Loop_number + 1]),
      timer:sleep(1000),
      pong ! (N-1),
      ping(Sum + N, Loop_number + 1)
  after
    20000 -> ok
  end.

pong() ->
  receive
    0 -> io:format("Pong otrzymal 0~n"), pong();
    stop -> ok;
    N ->
      io:format("Pong otrzymal ~p~n", [N]),
      timer:sleep(1000),
      ping ! (N-1),
      pong()
  after
    20000 -> ok
  end.

stop() ->
  ping ! stop,
  pong ! stop.


