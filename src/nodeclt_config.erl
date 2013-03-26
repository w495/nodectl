%% @copyright 2012 tvzavr.ru
%% @author Ilya w-495 Nikitin
%% @doc Configuration manager for erlang application.
%%

-module(nodeclt_config).

-define(DEFAULT_CFG_STORE, default_cfg_store).

-define(EXTENSION, ".config").

-export([
    start/1,
    start/2,
    reload/1,
    reload/2,
    init/0,
    deinit/0,
    deinit/1,
    put/2,
    put/3,
    get/2,
    get/3,
    store/1,
    store/2,
    rewrite/3
]).

%%-----------------------------------------------------------------------------
%% API
%%-----------------------------------------------------------------------------

start(App) ->
    start(App, []).

start(App, X)->
    reload(App, X).

init() ->
    init(cfg_store()).

init(App) ->
    init(App, []).

init(App, Opts) ->
    case ets:info(App) of
        undefined ->
            ets:new(App, [set, public, named_table|Opts]);
        _         ->
            App
    end.

put(App, Key,Val) ->
    application:set_env(App, Key,Val),
    true = ets:insert(App, {Key,Val}).

put(Key, Val) ->
    put(cfg_store(), Key,Val).

rewrite(Key,SubKey,Val) ->
    case nodeclt_config:get(Key, undefined) of
        L when is_list(L) ->
            L2 = proplists:delete(SubKey,L),
            nodeclt_config:put(Key, [{SubKey,Val}|L2]);
        _ -> nodeclt_config:put(Key,[{SubKey,Val}])   %% force
    end.

store(Args) when is_list(Args) ->
    store(cfg_store(), Args).

store(App, Args) when is_list(Args) ->
    lists:foreach(fun({Key,Val})->
        application:set_env(App, Key,Val)
    end, Args),
    true = ets:insert(App, Args).

get(Key, Default) ->
    get(cfg_store(), Key, Default).

get(App, Key, Default) ->
    case catch(ets:lookup(App, Key)) of
        {'EXIT',_} -> Default;
        [{Key,Val}] -> Val;
        _  -> Default
    end.

deinit() ->
    deinit(cfg_store()).

deinit(App) ->
    case ets:info(App) of
        undefined ->
            ok;
        _ ->
            ets:delete(App)
    end.

reload(App) ->
    reload(App, []).

reload(App, Cfg_procs) ->
    try
       init(App),
       Cfg_file = get_config_name(),
       Args = get_app_params(App, Cfg_file),
       store(App, Args),
       notify_procs(cfg_reloaded, Cfg_procs),
       after_reload(),
       [{config_file,Cfg_file}|Args]
    catch
        _:{error, Why} ->
            nodeclt_flog:error([{?MODULE,reload},{error,Why}]);
        _:Err ->
            nodeclt_flog:error([{?MODULE,reload},{error,Err}])
    end.


%%-----------------------------------------------------------------------------
%% INTERNAL
%%-----------------------------------------------------------------------------

get_config_name() ->
    case init:get_argument(config) of
        error ->
            throw({error,{nodeclt_config,undefined}});
        {ok,[[File]]} ->
            lists:append(File,  ?EXTENSION)
    end.

get_app_params(App, File) ->
    case file:consult(File) of
        {ok, [L]} ->
            proplists:get_value(App, L, []);
        Err -> throw(Err)
    end.

notify_procs(Msg, L) ->
    F = fun({Type,Proc}) ->
        case whereis(Proc) of
            Pid when is_pid(Pid) -> notify_proc(Type, Pid, Msg);
            _ -> ok
        end
    end,
    lists:foreach(F, L).

notify_proc(gen_server, Pid, Msg) ->
    gen_server:cast(Pid, Msg);
notify_proc(gen_event, Pid, Msg) ->
    gen_event:notify(Pid, Msg).


after_reload() ->
    handle_os_mon().

handle_os_mon() ->
    case nodeclt_config:get(os_mon, false) of
        true  ->
            application:start(os_mon, permanent);
        false ->
            application:stop(os_mon)
    end.

cfg_store() ->
    case application:get_application() of
        undefined ->    ?DEFAULT_CFG_STORE;
        {ok, App} ->    App
    end.


%% --------------------------------------------------------------------

