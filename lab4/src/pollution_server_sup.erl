%%%-------------------------------------------------------------------
%% @doc pollution_server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(pollution_server_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
        intensity => 5,
        period => 1},
    ChildSpec = #{id => pollution_gen_server,
        start => {pollution_gen_server, start_link, []},
        restart => permanent,
        shutdown => 2000,
        type => worker,
        modules => [pollution_gen_server]},
    {ok, {SupFlags, [ChildSpec]}}.

