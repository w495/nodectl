%% @copyright 2012 tvzavr.ru
%% @author Ilya w-495 Nikitin
%% Prev version:   27.04.2010, Andrew Gopienko
%% @doc Erlang application for automatically reloading modified modules
%% during developing and production.
%%

-module(nodeclt).

%%
%% Include files
%%
-include("nodeclt.hrl").  %% for APP & FMT macros

%%
%% Exported Functions
%%
-export([start/0, stop/0, call/2]).

%%
%% Defines
%%
-define( RPC_TIMEOUT, 10000 ).

%%
%% API Functions
%%
start() ->
    Rc = init:get_plain_arguments(),
    case Rc of
  [] ->
      usage();
  FullCommand ->
      {Node, Command, Args} = parse_args(FullCommand),
%%      io:format("Node:~p, Command:~p, Args=~p~n", [Node,Command,Args]),
      case catch action(Command, Node, Args) of
    ok ->
        io:format("done.~n"),
        halt(0);
    {'EXIT', {function_clause, [{?MODULE, action, _} | _]}} ->
        io:format("Invalid command ~p~n", [FullCommand]),
        usage();
    {'EXIT', {function_clause, [{ctl_app, action, _} | _]}} ->
        io:format("Invalid command ~p~n", [FullCommand]),
        usage();
    Other ->
        io:format("~naction ~p failed:~n~p~n",
                  [Command, Other]),
        halt(2)
      end
    end.

stop() ->
    ok.

%%
%% Local Functions
%%
parse_args(["-n", NodeS, Command | Args]) ->
    Node = case lists:member($@, NodeS) of
               true  -> list_to_atom(NodeS);
               false -> usage()
           end,
    {Node, list_to_atom(Command), Args};
parse_args(_) ->
    usage().


usage() ->
    AppHelp = case catch(ctl_app:usage()) of
            {'EXIT', _} -> "";
            Help -> Help
              end,
    io:format("Usage: ctl [-n <node>] <command> [<arg> ...]

Available commands for ~p application:

-- Manage --
  reload_code - hot code swap :-)
  reload_cfg  - reloads config
  status      - request application status
  stop        - stops application and halts the node
  stop_app    - stops application, leaving the node running
  start_app   - starts application on an already-running node
  version     - request application version
  upgrade Loc - upgrade application node(s) (send ebin and js/build dirs)
                  Loc - look for priv/cfg/upgrade/Loc.conf for params

-- Debug --
  log_level [Level] - logging level, Level = info|error|debug all|debug partial
                      returns current log level if Level is skipped
  debug_opts Opts   - log session debug messages for Opts,
                       Opts = phone Phone true|false | Module true|false

-- DB cluster --
  reset       - resets node to default configuration, deleting all data
  force_reset - force reset node
  cluster <ClusterNode> ... - add current node to cluster nodes
~s
",
    [?APP, AppHelp]),
    halt(1).
%
% -----------------------------------------------------------------------------
%% Manage commands
% -----------------------------------------------------------------------------
action(reload_code, Node, []) ->
    io:format("Reloading code on node ~p ... ", [Node]),
    Res = call(Node, {nodeclt_mgr, reload_code, []}),
    io:format("~n~p~n", [Res]),
    ok;

action(reload_cfg, Node, []) ->
    io:format("Reloading config on node ~p ... ", [Node]),
    Res = call(Node, {nodeclt_mgr, reload_cfg, []}),
    io:format("~n~p~n", [Res]),
    ok;

action(status, Node, []) ->
    Start = utime(),
    io:format("Status of node ~p ... ", [Node]),
    Res = call(Node, {nodeclt_mgr, status, []}),
    io:format("~n~p~n  time: ~p us~n", [Res, utime()-Start]),
    ok;

action(stop, Node, []) ->
    io:format("Stopping and halting node ~p ... ", [Node]),
    call(Node, {nodeclt_mgr, stop_and_halt, []});

action(stop_app, Node, []) ->
    io:format("Stopping ~p application on node ~p ... ", [?APP, Node]),
    call(Node, {?APP, stop, []});

action(start_app, Node, []) ->
    io:format("Starting ~p application on node ~p ... ", [?APP, Node]),
    call(Node, {?APP, start, []});

action(version, Node, []) ->
    io:format("Request version from node ~p ...~n", [Node]),
    Res = call(Node, {nodeclt_mgr, version, []}),
    io:format("~p~n", [Res]);

action(upgrade, _Node, Location) ->
    Conf = ?FMT("priv/cfg/upgrade/~s.conf", [Location]),
    case file:consult(Conf) of
  {ok, [Params]} ->
      nodeclt_utils:do_upgrade(Params);
  {error, enoent} ->
      io:format("Upgrade failed, no config file ~s~n", [Conf]);
  {error, Reason} ->
      io:format("Upgrade failed, ~p~n", [Reason])
    end;

% -----------------------------------------------------------------------------
% Debug
% -----------------------------------------------------------------------------
action(log_level, Node, []) ->
    io:format("Get session logging level on node ~p ...~n",
              [Node]),
    Res = rpc_call(Node, nodeclt_mgr, log_level, [<<>>], 5000),
    io:format("~nCurrent log level: ~p~n", [Res]),
    ok;
action(log_level, Node, Args) ->
    io:format("Set session logging level ~p on node ~p ...~n",
              [Args, Node]),
    BinArgs = lists:map(fun list_to_binary/1, Args),
    rpc_call(Node, nodeclt_mgr, log_level, [BinArgs], infinity);

action(debug_opts, Node, Args) ->
    io:format("Set session debug flag ~p on node ~p ...~n",
              [Args, Node]),
    BinArgs = lists:map(fun list_to_binary/1, Args),
    rpc_call(Node, nodeclt_mgr, debug_opts, [BinArgs], infinity);

% -----------------------------------------------------------------------------
% Cluster
% -----------------------------------------------------------------------------
action(Cmd, Node, Args) ->
    case catch(ctl_app:action(Cmd, Node, Args)) of
  {'EXIT', {undef, _}} -> %% no app specific actions
      {'EXIT', {function_clause, [{?MODULE, action, dummy}]}};
  Other -> Other
    end.

% -----------------------------------------------------------------------------
call(Node, {Mod, Fun, Args}) ->
    rpc_call(Node, Mod, Fun, lists:map(fun list_to_binary/1, Args)).

rpc_call(Node, Mod, Fun, []) ->
    rpc:call(Node, Mod, Fun, [], ?RPC_TIMEOUT);
rpc_call(Node, Mod, Fun, Args) ->
    rpc:call(Node, Mod, Fun, [Args], ?RPC_TIMEOUT).

rpc_call(Node, Mod, Fun, Args, Timeout) ->
    rpc:call(Node, Mod, Fun, Args, Timeout).

% -----------------------------------------------------------------------------
% now() to seconds + microseconds
utime() ->
    {_M,S,Mi} = now(),
    S*1000000+Mi.
