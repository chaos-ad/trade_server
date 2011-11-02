-module(trade_strategy_manager).
-behaviour(supervisor).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start_link/0, init/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(SERVER, {global, ?MODULE}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

accounts() ->
    [ {Name, Pid} || {Name, Pid, _, _} <- supervisor:which_children(?SERVER) ].

account_pid(Account) ->
    case lists:keyfind(Account, 1, accounts()) of
        false  -> undefined;
        Result -> element(2, Result)
    end.

strategies(Account) ->
    trade_strategy_manager_sup:strategies(account_pid(Account)).

strategy_pid(Account, Strategy) ->
    case lists:keyfind(Strategy, 1, strategies(Account)) of
        false  -> undefined;
        Result -> element(2, Result)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

stop_account(Account) ->
    supervisor:terminate_child(?SERVER, Account).

start_account(Account) ->
    supervisor:restart_child(?SERVER, Account).

add_account(Account, StrategiesList) ->
    supervisor:start_child(?SERVER, child_spec(Account, StrategiesList)).

del_account(Account) ->
    supervisor:delete_child(?SERVER, Account).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

stop_strategy(Account, Module) ->
    trade_strategy_manager_sup:stop_strategy(account_pid(Account), Account, Module).

start_strategy(Account, Module) ->
    trade_strategy_manager_sup:start_strategy(account_pid(Account), Account, Module).

add_strategy(Account, Module, Args) ->
    trade_strategy_manager_sup:add_strategy(account_pid(Account), Account, Module, Args).

del_strategy(Account, Module) ->
    trade_strategy_manager_sup:del_strategy(account_pid(Account), Account, Module).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    supervisor:start_link(?SERVER, ?MODULE, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
    {ok, StrategiesSpec} = application:get_env(strategies),
    RestartStrategy = {one_for_one, 5, 600},
    Children = [ child_spec(Acc, List) || {Acc, List} <- StrategiesSpec ],
    {ok, {RestartStrategy, Children}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

child_spec(Account, StrategiesList) ->
    {Account,
        {trade_strategy_manager_sup, start_link, [[Account, StrategiesList]]},
        permanent, 60000, supervisor, [Account]
    }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

