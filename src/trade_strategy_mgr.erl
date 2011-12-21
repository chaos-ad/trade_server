-module(trade_strategy_mgr).
-behaviour(supervisor).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start_link/0, init/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(SERVER, {global, ?MODULE}).
-define(CHILD(I), {I, {I, start_link, []}, permanent, 60000, worker, [I]}).
-define(CHILD(I, Args), {I, {I, start_link, Args}, permanent, 60000, worker, [I]}).
-define(CHILD(I, Args, Role), {I, {I, start_link, Options}, permanent, 60000, Role, [I]}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    supervisor:start_link(?SERVER, ?MODULE, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

strategies() ->
    [ {Name, Pid} || {Name, Pid, _, _} <- supervisor:which_children(?SERVER) ].

strategy(Name) ->
    case lists:keyfind(Name, 1, strategies()) of
        {Name, Pid} -> Pid;
        undefined   -> undefined
    end.

stop_strategy(Strategy) ->
    supervisor:terminate_child(?SERVER, Strategy).

start_strategy(Strategy) ->
    supervisor:restart_child(?SERVER, Strategy).

add_strategy(Strategy) ->
    supervisor:start_child(?SERVER, child_spec(Strategy)).

del_strategy(Strategy) ->
    supervisor:delete_child(?SERVER, Strategy).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
    RestartStrategy = {one_for_one, 2, 600},
    Children = lists:map(fun child_spec/1, get_strategies()),
    {ok, {RestartStrategy, Children}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

child_spec({Name, Terminal, Module, Options}) ->
    StartFunc = {trade_strategy, start_link, [Name, Terminal, Module, Options]},
    {Name, StartFunc, permanent, 60000, worker, [trade_strategy]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_strategies() ->
    case application:get_env(strategies) of
        {ok, Strategies} -> Strategies;
        _              -> []
    end.
