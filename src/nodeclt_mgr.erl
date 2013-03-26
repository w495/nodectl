%% @copyright 2012 tvzavr.ru
%% @author Ilya w-495 Nikitin
%% @doc Erlang application manager for automatically reloading modified modules
%% during developing and production.
%%

-module(nodeclt_mgr).

-include("nodeclt.hrl").

%%% --------------------------------------------------------------------
%%% API
%%% --------------------------------------------------------------------
-export([start/1, stop/1, stop_and_halt/0,
         status/0, version/0,
         start_applications/1, stop_applications/1,
         reload_code/0,
         reload_cfg/0,
         log_level/1,
         debug_opts/1]).

%%% --------------------------------------------------------------------
%%% Macros
%%% --------------------------------------------------------------------

start(StartApps) ->
    try
        Apps = StartApps  ++ [application:get_application()],
        ok = start_applications(Apps)
    after
      %% give the error loggers some time to catch up
      timer:sleep(100)
    end.


stop(StopApps) ->
    Apps = StopApps ++ [application:get_application()],
    stop_applications(Apps),
    ok.

stop_and_halt() ->
    spawn(fun() ->
            SleepTime = 1000,
%            nodeclt_flog:info([{?MODULE,"received stop_and_halt"}]),
            timer:sleep(SleepTime),
            halt(0)
          end),
    case catch stop([application:get_application()]) of _ -> ok end.


status() ->
    nodeclt_utils:status().


version() -> {0,2}.


%% ---------------------------------------------------------------------------
log_level(<<>>) ->
    nodeclt_config:get(log_level, info);
log_level(BinArgs) ->
    Args = lists:map(fun binary_to_list/1, BinArgs),
    case Args of
  ["info"]  -> nodeclt_config:put(log_level, info);   %% info + errors
  ["error"] -> nodeclt_config:put(log_level, error);  %% no info, only errors
  ["debug","all"] -> nodeclt_config:put(log_level, {debug,all}); %% info + errors + all debug events
  ["debug","partial"] -> nodeclt_config:put(log_level, {debug,partial}); %% info + errors + some debug events
  Other -> {error, {badarg, Other}}
    end.

debug_opts(BinArgs) ->
    Args = lists:map(fun binary_to_list/1, BinArgs),
    case Args of
  ["phone", Phone, "true"]  -> nodeclt_config:rewrite({phone,Phone}, debug, true);
  ["phone", Phone, "false"] -> nodeclt_config:rewrite({phone,Phone}, debug, false);
  [Module, "true"]  -> nodeclt_config:put(nodeclt_utils:to_atom(Module), true);
  [Module, "false"] -> nodeclt_config:put(nodeclt_utils:to_atom(Module), false);
  Other -> {error, {badarg, Other}}
    end.


% -----------------------------------------------------------------------------
reload_code() ->
    nodeclt_reloaderf:reload_code().

%% ---------------------------------------------------------------------------
reload_cfg() ->
    lists:foreach(fun({Application, _description, _vsn})->
        nodeclt_config:reload(Application, ?CFG_PROCS)
    end, application:which_applications()).


%%% ============================================================================
%%% APP manage
%%% ============================================================================
manage_applications_s(Iterate, Do, Undo, SkipError, ErrorTag, Result, Apps) ->
    Iterate(fun(App,Acc) ->
                case Do(App,permanent) of
              ok -> [{App,Result} | Acc];
              {error, {SkipError, _}} -> Acc;
              {error, Reason} ->
                  lists:foreach(Undo, Acc),
                  throw({error, {ErrorTag, App, Reason}})
                end
            end, [], Apps),
    ok.


manage_applications_e(Iterate, Do, Undo, SkipError, ErrorTag, Result, Apps) ->
    Iterate(fun(App,Acc) ->
                case Do(App) of
              ok -> [{App,Result} | Acc];
              {error, {SkipError, _}} -> Acc;
              {error, Reason} ->
                  lists:foreach(Undo, Acc),
                  throw({error, {ErrorTag, App, Reason}})
                end
            end, [], Apps),
    ok.


start_applications(Apps) ->
    manage_applications_s(fun lists:foldl/3,
                          fun application:start/2,
                          fun application:stop/1,
                          already_started,
                          cannot_start_application,
                          started,
                          Apps).

stop_applications(Apps) ->
    manage_applications_e(fun lists:foldr/3,
                          fun application:stop/1,
                          fun application:start/1,
                          not_started,
                          cannot_stop_application,
                          stopped,
                          Apps).
