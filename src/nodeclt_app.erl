%% @copyright 2012 tvzavr.ru
%% @author Ilya w-495 Nikitin
%% @doc Erlang application for automatically reloading modified modules
%% during developing and production.
%%

-module(nodeclt_app).
-behaviour(application).
-export([start/0, start/2, stop/1]).

start() ->
    application:start(nodeclt).

start(_Type, _Args) ->
    nodeclt_sup:start_link().

    
stop(_State) ->
    ok.
