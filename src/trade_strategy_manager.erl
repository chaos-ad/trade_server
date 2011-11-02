-module(trade_strategy_manager).
-behaviour(supervisor).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start_link/0, init/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(SERVER, {global, ?MODULE}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

status() ->
    supervisor:which_children(?SERVER).

find(Account) ->
    case lists:keyfind(Account, 1, status()) of
        false  -> undefined;
        Result -> element(2, Result)
    end.

start(Account) ->
    supervisor:start_child(?SERVER, Account).

terminate(Account) ->
    supervisor:terminate_child(?SERVER, Account).

restart(Account) ->
    supervisor:restart_child(?SERVER, Account).

start_link() ->
    supervisor:start_link(?SERVER, ?MODULE, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
    {ok, StrategiesSpec} = application:get_env(strategies),
    RestartStrategy = {one_for_one, 5, 600},
    Children = [
        {Account, {trade_strategy_manager_sup, start_link, [[Account, StratList]]},
            permanent, 60000, supervisor, [Account]} ||
        {Account, StratList} <- StrategiesSpec
    ],
    lager:debug("Starting strategy manager with spec: ~p", [Children]),
    {ok, {RestartStrategy, Children}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
