%% @copyright 2012 tvzavr.ru
%% @author Ilya w-495 Nikitin
%% @doc Erlang module for automatically reloading modified modules
%% during production.
%% 
-module(nodeclt_reloaderf).

-export([reload_code/0]).

reload(Module) ->
    case catch sys:suspend(Module) of
        {'EXIT',{noproc,{sys,suspend,[Module]}}} ->
            reload_(Module);
        {'EXIT',{timeout,{sys,suspend,[Module]}}} ->
            reload_(Module);
        ok ->
            ok = sys:suspend(Module),
            X = reload_(Module),
            ok = sys:change_code(Module,Module, [],[]),
            ok = sys:resume(Module),
            X
    end.
    
reload_(Module) ->
    code:purge(Module),
    code:soft_purge(Module),
    {module, Module} = code:load_file(Module),
    {ok,Module}.
    
reload_code() ->
    try
        %      Modules = [M || {M,P} <- code:all_loaded(),
        %                               is_list(P) andalso
        %                               string:str(P, filename:absname(""))>0],
        Modules = modified_modules(),
        {ok, lists:sort([reload(M) || M <- Modules])}
    catch
        Cls:Why -> {error, {Cls,Why}}
    end.

modified_modules() ->
    [M || {M, P} <- code:all_loaded(),
                    is_list(P),
                    string:str(P, filename:absname(""))>0,
                    module_modified(M) == true].

module_modified(Module) ->
    case code:is_loaded(Module) of
        {file, preloaded} ->
            false;
        {file, Path} ->
            Compile_opts = proplists:get_value(compile, Module:module_info()),
            Compile_time = proplists:get_value(time, Compile_opts),
            Src = proplists:get_value(source, Compile_opts),
            module_modified(Path, Compile_time, Src);
        _ ->
            false
    end.

module_modified(Path, PrevCompileTime, PrevSrc) ->
    case find_module_file(Path) of
        false ->
            false;
        Mod_path ->
            {ok, {_, [{_, CB}]}} = beam_lib:chunks(Mod_path, ["CInf"]),
            Compile_opts = binary_to_term(CB),
            Compile_time = proplists:get_value(time, Compile_opts),
            Src = proplists:get_value(source, Compile_opts),
            not ((Compile_time == PrevCompileTime) and (Src == PrevSrc))
    end.

find_module_file(Path) ->
    case file:read_file_info(Path) of
        {ok, _} ->
            Path;
        _ ->
            %% may be the path was changed?
            case code:where_is_file(filename:basename(Path)) of
                non_existing ->
                    false;
                NewPath ->
                    NewPath
            end
    end.
