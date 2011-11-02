-module(trade_strategy_manager_sup).
-behavior(supervisor).
-compile(export_all).

-export([init/1, start_link/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

strategies(undefined) -> {error, account_not_started};
strategies(Pid) ->
    [ {Name, P} || {{_, Name}, P, _, _} <- supervisor:which_children(Pid) ].

stop_strategy(undefined, _, _) -> {error, account_not_started};
stop_strategy(Pid, Account, Module) ->
    supervisor:terminate_child(Pid, make_id(Account, Module)).

start_strategy(undefined, _, _) -> {error, account_not_started};
start_strategy(Pid, Account, Module) ->
    supervisor:restart_child(Pid, make_id(Account, Module)).

add_strategy(undefined, _, _, _) -> {error, account_not_started};
add_strategy(Pid, Account, Module, Args) ->
    supervisor:start_child(Pid, child_spec(Account, Module, Args)).

del_strategy(undefined, _, _) -> {error, account_not_started};
del_strategy(Pid, Account, Module) ->
    supervisor:delete_child(Pid, make_id(Account, Module)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Args) ->
    supervisor:start_link(?MODULE, Args).

init([Account, StratList]) ->
    RestartStrategy = {one_for_one, 5, 600},
    Children = [child_spec(Account, Name, Args) || {Name, Args} <- StratList],
    {ok, {RestartStrategy, Children}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

child_spec(Account, Module, Args) ->
    ID = make_id(Account, Module),
    StartFunc = {Module, start_link, [Account, Args]},
    {ID, StartFunc, permanent, 60000, supervisor, [Module]}.

make_id(Account, Module) ->
    {Account, Module}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
