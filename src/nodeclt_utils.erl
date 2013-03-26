%% @copyright 2012 tvzavr.ru
%% @author Ilya w-495 Nikitin
%% @doc Erlang application utils for automatically reloading modified modules
%% during developing and production.
%%

-module(nodeclt_utils).

-export([safety_call/2,safety_call/3,utime/0,udelta/2,make_ets/1,myFmt/3,drop_ets/1,
         getAppParam/2, parse_args/1, pick/3, s2i/2, get_param/3,
         swap/2, to_atom/1, to_int/1, to_list/1, whereis_phone/1,
         ugs/0, to_integer/1, make_ets/2, to_float/1,
         to_utf32/1,
         reload_cfg/2, reload_code/0,
         start_applications/1, stop_applications/1,
         start_childs/2, stop_dynamic_child/2,
         start_groups/1, stop_groups/1,
         status/0, status/1, os_stat/1,
         upgrade/1, do_upgrade/1,
         unixtime_to_localDatetime/1, unixtime_to_universalDatetime/1,
         hex_to_binary/1, to_hex/1, hex_to_list/1, setPlistVal/2]).

-define( FMT(F,P), lists:flatten(io_lib:format(F,P)) ).

%%-include("../include/db.hrl").

%% --------------------------------------------------------------------
safety_call(Info, F) ->
    try
      F()
    catch
  _:{error, Why} -> nodeclt_flog:error(Info ++ [{error,Why},
                                        {stack,erlang:get_stacktrace()}]),
                    {error,Why};
  _:Err          -> nodeclt_flog:error(Info ++ [{unexpected_error,Err},
                                        {stack,erlang:get_stacktrace()}]),
                    {error,Err}
    end.

%% --------------------------------------------------------------------
safety_call(Info, F, ErrReturn) ->
    try
      F()
    catch
  _:{error, Why} -> nodeclt_flog:error(Info ++ [{error,Why}]),            ErrReturn;
  _:Err          -> nodeclt_flog:error(Info ++ [{unexpected_error,Err}]), ErrReturn
    end.

%% --------------------------------------------------------------------
% now() to seconds + microseconds
utime() ->
    {_M,S,Mi} = now(),
%    {_M,S,Mi} = os:timestamp(),
    S*1000000+Mi.


udelta({M1, S1, Mi1}, {M2, S2, Mi2}) ->
    (M1 - M2) * 1000000000000 + (S1 - S2) * 1000000 + (Mi1 - Mi2).
%% -----------------------------------------------------------------------------
ugs() ->
    calendar:datetime_to_gregorian_seconds(erlang:universaltime()).

%% --------------------------------------------------------------------
make_ets(Name) -> make_ets(Name, []).

make_ets(Name, Opts) ->
    case ets:info(Name) of
  undefined -> ets:new(Name, [set, public, named_table|Opts]);
  _         -> Name
    end.

drop_ets(Name) ->
    case ets:info(Name) of
  undefined -> ok;
  _         -> ets:delete(Name)
    end.

%% --------------------------------------------------------------------
myFmt([Ht|Txt], [Hp|Params], Ret) when is_list(Hp) /= true->
    myFmt(Txt, Params, Ret ++ Ht ++ sms:to_list(Hp));
myFmt([Ht|Txt], [Hp|Params], Ret) ->
    myFmt(Txt, Params, Ret ++ Ht ++ Hp);
%    myFmt(Txt, Params, <<Ret/binary, Ht/binary, Hp/binary>>);
myFmt([Ht|Txt], [], Ret) ->
    myFmt(Txt, [], Ret ++ Ht);
%    myFmt(Txt, [], <<Ret/binary, Ht/binary>>);
myFmt([], _X, Ret) ->
    Ret.

%% --------------------------------------------------------------------
getAppParam(Key, Default) ->
    case application:get_env(Key) of
  {ok,Val} -> Val;
  _        -> Default
    end.

%% --------------------------------------------------------------------
parse_args(L) when is_list(L) ->
    lists:map(fun(E) ->
                    case string:tokens(E, "=") of
                  [Name,Val] -> {string:to_lower(Name),Val};
                  _          -> {bad_arg,E}
                    end
              end, L);
parse_args(L) ->
    nodeclt_flog:info([{?MODULE,parse_args},{bad_arg,L}]),
    [].

% -----------------------------------------------------------------------------
pick(Key, List, Default) ->
    case lists:keysearch(Key, 1, List) of
  false when Default == must ->
      {error, badarg};
  false ->
      Default;
  {value, {Key, Value}} ->
      Value;
  {value, BadArg} ->
      {error, {bad_type, BadArg}}
    end.

% -----------------------------------------------------------------------------
s2i(Name, S) ->
    case catch(list_to_integer(S)) of
  {'EXIT', _Why} ->
      throw({error,{bad_int_str, {Name,S}}});
  Val -> Val
    end.

% -----------------------------------------------------------------------------
get_param(Key, Params, Default) ->
    case pick(Key, Params, []) of
  []  -> Default;
  Val -> Val
    end.

% -----------------------------------------------------------------------------
swap([], Acc) -> lists:reverse(Acc);
swap([B1,B2|Tail], Acc) ->
    swap(Tail, [B1,B2|Acc]);
swap(_, Acc) -> swap([], Acc).


%% --------------------------------------------------------------------
whereis_phone(Phone) when is_atom(Phone) ->
   whereis(Phone);
whereis_phone(Phone) when is_list(Phone) ->
   whereis(to_atom(Phone));
whereis_phone(Phone) when is_integer(Phone) ->
   whereis_phone(integer_to_list(Phone)).

%% --------------------------------------------------------------------
to_integer(V) when is_list(V) ->
    list_to_integer(V);
to_integer(V) ->
    V.

%% --------------------------------------------------------------------
to_atom(Val) when is_atom(Val) -> Val;
to_atom(Val) when is_list(Val) ->
    case catch(erlang:list_to_existing_atom(Val)) of
  {'EXIT', _} -> erlang:list_to_atom(Val);
  Atom -> Atom
    end;
to_atom(Val) when is_integer(Val) ->
    to_atom(integer_to_list(Val));
to_atom(Val) when is_binary(Val) ->
    to_atom(binary_to_list(Val)).


to_int(Val) when is_integer(Val) -> Val;
to_int(Val) when is_list(Val) ->
    case catch(erlang:list_to_integer(Val)) of
  {'EXIT', _} -> throw({error, {to_int, bad_arg, Val}});
  Int -> Int
    end;
to_int(Val) when is_atom(Val) ->
    to_int(atom_to_list(Val));
to_int(Val) when is_binary(Val) ->
    case catch(list_to_integer(binary_to_list(Val))) of
  {'EXIT', _} -> throw({error, {to_int, bad_arg, Val}});
  Int -> Int
    end.



to_list(Val) when is_list(Val) -> Val;
to_list(Val) when is_atom(Val) ->
    atom_to_list(Val);
to_list(Val) when is_integer(Val) ->
    integer_to_list(Val);
to_list(Val) when is_binary(Val) ->
    binary_to_list(Val).


to_float(Val) when is_list(Val) ->
    case catch(erlang:list_to_float(Val)) of
        {'EXIT', _} ->
            case catch(erlang:list_to_integer(Val)) of
                {'EXIT', _} -> throw({error, {to_float, bad_arg, Val}});
                Int -> Int
            end;
        Float -> Float
    end;
to_float(Val) when is_float(Val); is_integer(Val) ->
    Val.

to_utf32(Data) -> {utf32, to_utf32_impl(Data)}.

to_utf32_impl({ucs2,Sms}) when is_list(Sms) -> %% исходящие СМС
    unicode:characters_to_list(list_to_binary(Sms), utf8);
to_utf32_impl({ascii,Sms}) when is_list(Sms) -> %% исходящие СМС
    Sms;
to_utf32_impl({ucs2in,Sms}) when is_list(Sms) -> %% входящие СМС
    unicode:characters_to_list(list_to_binary(Sms), {utf16, big});
to_utf32_impl(Sms) when is_list(Sms) ->
    unicode:characters_to_list(list_to_binary(Sms), utf8);
to_utf32_impl(Sms) ->
    ?FMT("~p",[Sms]).



% -----------------------------------------------------------------------------
reload_cfg(App, CfgProcs) ->
    nodeclt_config:reload(App, CfgProcs).

% -----------------------------------------------------------------------------
reload_code() ->
    nodeclt_reloaderf:reload_code().

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

start_childs(_Sup, []) -> ok;
start_childs(Sup, [H|T]) ->
    {ok,_} = supervisor:start_child(Sup, [H]),
    start_childs(Sup, T).

%% ----------------------------------------------------------------------------
stop_dynamic_child(Sup, Id) ->
    ok = supervisor:terminate_child(Sup, Id),
    ok = supervisor:delete_child(Sup, Id),
    ok.

%% ----------------------------------------------------------------------------
start_groups(both) ->
    start_group(tracker),
    start_group(customer);
start_groups(Other) ->
    start_group(Other).

start_group(GateType) ->
    GroupName = get_group_name(GateType),
    pg2:create(GroupName),
    case pg2:join(GroupName, self()) of
  ok -> ok;
  {error,Reason} -> nodeclt_flog:error([{?MODULE,start_group},{join_failed,Reason}])
    end.

stop_groups(both) ->
    pg2:leave(smpp_gate_tracker, self()),
    pg2:leave(smpp_gate_customer, self());
stop_groups(GateType) ->
    GroupName = get_group_name(GateType),
    pg2:leave(GroupName, self()).

get_group_name(customer) -> smpp_gate_customer;
get_group_name(_)        -> smpp_gate_tracker.

%% ----------------------------------------------------------------------------
status() ->
    [{running_applications, application:which_applications()}].

status(Caller) ->
    Apps = application:which_applications(),
    Memory  = erlang:memory(),
    Events  = get_events_stat(),
    {ok, fmt_status(Caller, Apps,Memory, Events)}.

get_events_stat() ->
    case ets:info(tran_stat, size) of
  undefined -> [];
  _ ->
      RWS =
          case ets:lookup(tran_stat, write_stat) of
        [{write_stat,{WTime,Size,Time}}] ->
            Rate = if Time > 0 -> {round(Size/Time*1000000),eps};
                   true -> {0,eps} end,
            {WTime,Size,Time,Rate};
        [] -> no_stat
          end,
      FDS = case ets:lookup(tran_stat, last_dump) of
        [] -> no_stat;
        [{last_dump,{Tab1,Rc1,Size1,Time1}}] ->
            FDRate = if Time1 > 0 -> {round(Size1/Time1*1000000),eps};
                     true -> {0,eps} end,
            {Tab1,Rc1,Size1,Time1,FDRate}
        end,
      BS = case ets:lookup(tran_stat, backup_stat) of
         [] -> no_stat;
         [{backup_stat, {Tab2, Size2, Time2}}] ->
             BRate = if Time2 > 0 -> {round(Size2/Time2*1000000),eps};
                     true -> {0,eps} end,
             {Tab2,Size2,Time2,BRate}
          end,
      [RWS, FDS, BS]
    end.

os_stat(Caller) ->
    {_,Util,_,_} = cpu_sup:util([]),
    Cpu = {Util,
           [cpu_sup:avg1()/256,
            cpu_sup:avg5()/256,
            cpu_sup:avg15()/256]},
    MemTot =
      list_to_integer(
        string:strip(
                   os:cmd("grep -i memtotal /proc/meminfo | awk '{print $2}'"),
                   right, $\n))/1024,  % MB
    Cmd = "grep -i '^active' /proc/meminfo | grep -v file | "
          "awk 'BEGIN {mem=0} {mem+=$2} END {print mem} '",
    MemAct =
      list_to_integer(
        string:strip( os:cmd(Cmd) ,
%                os:cmd("grep -i \"^active:\" /proc/meminfo | awk '{print $2}'"),
                right, $\n))/1024,  % MB
    MemErl   = memsup:get_system_memory_data(),
    SwapTotBytes = proplists:get_value(total_swap, MemErl),
    SwapTot = SwapTotBytes/(1024*1024), % MB
    SwapActBytes = SwapTotBytes - proplists:get_value(free_swap, MemErl),
    SwapAct = SwapActBytes/(1024*1024), % MB
    Mem = {MemTot, MemAct, SwapTot, SwapAct},
    Disk = disksup:get_disk_data(),
    {ok, fmt_os_stat(Caller, Cpu, Mem, Disk)}.

fmt_os_stat(mochi, {Util,Avg},
                  {MemTot, MemAct, SwapTot, SwapAct}, Disk) ->
    Cpu = {struct, [{util, list_to_float(?FMT("~.2f", [Util]))},
                    {la, list_to_binary(?FMT("~.2f, ~.2f, ~.2f", Avg))}
                   ]},
    MemUtil = case (MemAct*100)/MemTot of
            Val when Val < 40 -> <<"low">>;
            Val when Val < 80 -> <<"medium">>;
            _                 -> <<"high">>
              end,
    Mem = {struct, [{total, list_to_binary(?FMT("~.2f", [MemTot]))},
                    {active, list_to_binary(?FMT("~.2f", [MemAct]))},
                    {mem_util, MemUtil},
                    {swap_total, list_to_binary(?FMT("~.2f", [SwapTot]))},
                    {swap_active, list_to_binary(?FMT("~.2f", [SwapAct]))}
                   ]},
    Dsk = lists:foldl(fun({Path,Alloc,Perc}, Acc) ->
                          DskAlloc =
                              list_to_binary(?FMT("~.2f", [Alloc/(1024*1024)])),
                          [[list_to_binary(Path),DskAlloc,Perc]|Acc]
                      end, [], Disk),
    {struct, [{cpu, Cpu},{mem,Mem},{disk,lists:sort(Dsk)}]};
fmt_os_stat(_, {Util,Avg}, {MemTot, MemAct, SwapTot, SwapAct}, Dsk) ->
    [{cpu, [{util, Util},{la,Avg}]},
     {mem, [{total, MemTot}, {active, MemAct},
            {swap_total, SwapTot}, {swap_active, SwapAct}]},
     {disk,Dsk}].


fmt_status(mochi, Apps, Memory, EventsRaw) ->
    F = fun({Name,Desc,Version}, Acc) ->
            [[Name,list_to_binary(Desc),list_to_binary(Version)]|Acc]
    end,
    AppsNew = lists:reverse(lists:foldl(F, [], Apps)),
    Events = case EventsRaw of
           [] -> [];
           _ -> fmt_events(EventsRaw)
             end,
    {struct, [{node,list_to_binary(utils:to_list(node()))},
              {apps, AppsNew}, {mem,{struct,Memory}} | Events ]};
fmt_status(_, Apps, Memory, Events) ->
    [{apps, Apps},{mem,Memory},
     {events,Events}].


fmt_events([RWS, FDS, BS]) ->
    RWSJ = fmt_ev_write(RWS),
    FDSJ = fmt_ev_dump(FDS),
    BSJ  = fmt_ev_backup(BS),
    [{events, {struct,[RWSJ, FDSJ, BSJ]}}].

fmt_ev_write(no_stat) ->
    Time = mk_date_str(erlang:localtime()),
    {write, {struct, [{no_stat, Time}]}};
fmt_ev_write({LocalTime, Size, TimeUs, {Rate,_}}) ->
    Time = mk_date_str(LocalTime),
    {write,
      {struct, [{timestamp, Time},
                {events, Size},
                {time, list_to_binary(?FMT("~.3f ms", [TimeUs/1000]))},
                {rate, list_to_binary(?FMT("~p events/sec", [Rate]))}
               ]
      }
    }.

fmt_ev_dump(no_stat) ->
    Time = mk_date_str(erlang:localtime()),
    {dump, {struct, [{no_stat, Time}]}};
fmt_ev_dump({Tab,Rc,Size,TimeUs,{Rate,_}}) ->
    {dump,
      {struct, [{table, Tab},
                {result, list_to_binary(?FMT("~p", [Rc]))},
                {events, Size},
                {time, list_to_binary(?FMT("~.3f ms", [TimeUs/1000]))},
                {rate, list_to_binary(?FMT("~p events/sec", [Rate]))}
               ]
      }
    }.

fmt_ev_backup(no_stat) ->
    Time = mk_date_str(erlang:localtime()),
    {backup, {struct, [{no_stat, Time}]}};
fmt_ev_backup({Tab,Size,TimeUs,{Rate,_}}) ->
    {backup,
      {struct, [{table, Tab},
                {events, Size},
                {time, list_to_binary(?FMT("~.3f ms", [TimeUs/1000]))},
                {rate, list_to_binary(?FMT("~p events/sec", [Rate]))}
               ]
      }
    }.


mk_date_str({{Y,M,D},{H,Min,S}}) ->
    list_to_binary(?FMT("~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",
                        [Y,M,D,H,Min,S])).

%% ----------------------------------------------------------------------------
%% Upgrades nodes
%% ----------------------------------------------------------------------------
upgrade(Nodes, Paths) when is_list(Nodes), is_list(Paths) ->
    try
        FileName = "up.tar.bz2",
        ok = make_archive(FileName, Paths),
        {ok, FileBin} = file:read_file(FileName),
        send_upgrade(Nodes, FileBin)
    catch
  _:{error,Reason} -> {error,Reason};
  _:Error -> {error,Error}
    end.

upgrade(Bin) when is_binary(Bin) ->
    try
        FileName = "up.tar.bz2",
        ok = extract_archive(FileName,Bin),
        reload_code()
    catch
  _:{error,Reason} -> {error,Reason};
  _:Error -> {error,Error}
    end.

make_archive(FileName, Paths) ->
    F = fun(Dir, Acc) ->
            case filelib:is_dir(Dir) of
          true -> [Dir, Acc];
          false -> Acc
            end
    end,
    Cmd =
      case lists:foldl(F, [], Paths) of
    [] -> throw({error,{no_pathname, Paths}});
    L -> string:join(["tar cjf", FileName | L], " ")
      end,
    case os:cmd(Cmd) of
  [] -> ok;
  Error -> throw({error,Error})
    end.

extract_archive(FileName,Bin) ->
    ok = file:write_file(FileName, Bin),
    case os:cmd(?FMT("tar xmjf ~s", [FileName])) of
  [] -> ok;
  Error -> throw({error,Error})
    end.

send_upgrade(Nodes, Bin) ->
    F = fun(Node, Acc) ->
            Rc = rpc:call(Node, utils, upgrade, [Bin]),
            [{Node,Rc} | Acc]
    end,
    lists:reverse(lists:foldl(F, [], Nodes)).

%% ----------------------------------------------------------------------------
%% called from ctl
%% ----------------------------------------------------------------------------
do_upgrade(Params) ->
    case proplists:get_value(cookie, Params) of
  undefined -> ok; % same cookie
  Cookie -> erlang:set_cookie(node(), Cookie)
    end,
    case proplists:get_value(nodes, Params, []) of
  [] ->
      io:format("Upgrade failed, no nodes in nodeclt_config~n");
  Nodes ->
      Paths = case proplists:get_value(pathname, Params, []) of
            [] -> io:format("Upgrade failed, no pathname in nodeclt_config~n"),
                  throw({error,no_pathname_in_config});
            Val -> Val
              end,
      io:format("Upgrade nodes ~p with paths ~p ...~n", [Nodes, Paths]),
      Res = upgrade(Nodes, Paths),
      io:format("~p~n", [Res])
    end.

unixtime_to_localDatetime(T) ->
    X = T div 1000,
    calendar:now_to_local_time({X div 1000000,X rem 1000000,0}).

unixtime_to_universalDatetime(T) ->
    X = T div 1000,
    calendar:now_to_universal_time({X div 1000000,X rem 1000000,0}).



to_hex(Val) when is_list(Val) ->
    lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- Val]);
to_hex(Val) when is_binary(Val) ->
    to_hex(binary_to_list(Val)).


hex_to_binary(Val) ->
    list_to_binary(hex_to_list(Val)).


hex_to_list([X1, X2 | T], Res) ->
    hex_to_list(T, [erlang:list_to_integer([X1,X2], 16) | Res]);
hex_to_list([], Res) ->
    lists:reverse(Res).

hex_to_list(Val) ->
    hex_to_list(Val, []).
%    [erlang:list_to_integer(X, 16) || X <- Val].

setPlistVal([H|T], Plist) ->
    setPlistVal(T, setPlistVal(H, Plist));
setPlistVal([], Plist) ->
    Plist;
setPlistVal({Key, Val}, Plist) ->
    [{Key, Val} | proplists:delete(Key, Plist)].

