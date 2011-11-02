-module(trade_strategy_manager_sup).
-behavior(supervisor).
-compile(export_all).

-export([init/1, start_link/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Args) ->
    supervisor:start_link(?MODULE, Args).

init([Account, StratList]) ->
    RestartStrategy = {one_for_one, 5, 600},
    Children = [child_spec(Account, Name, Args) || {Name, Args} <- StratList],
    {ok, {RestartStrategy, Children}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

child_spec(Account, Module, Args) ->
    ID = {Account, Module},
    StartFunc = {Module, start_link, [Account, Args]},
    {ID, StartFunc, permanent, 60000, supervisor, [Module]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
