%% @copyright 2012 tvzavr.ru
%% @author Ilya w-495 Nikitin
%% @doc Erlang application for automatically reloading modified modules
%% during developing and production.
%%

-module(nodeclt_sup).
-behaviour(supervisor).

-export([start_link/0]). %% API.
-export([init/1]). %% supervisor.

-define(SUPERVISOR, ?MODULE).

%% API.

-spec start_link() -> {ok, Pid::pid()}.
start_link() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

%% supervisor.

init([]) ->
    {ok, {{one_for_one, 10, 10}, []}}.
