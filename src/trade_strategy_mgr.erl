-module(trade_strategy_mgr).
-behaviour(supervisor).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start_link/2, init/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(CHILD(I), {I, {I, start_link, []}, permanent, 60000, worker, [I]}).
-define(CHILD(I, Args), {I, {I, start_link, Args}, permanent, 60000, worker, [I]}).
-define(CHILD(I, Args, Role), {I, {I, start_link, Args}, permanent, 60000, Role, [I]}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Account, Strategies) ->
    supervisor:start_link(?MODULE, {Account, Strategies}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init({Account, Strategies}) ->
    {ok, { {one_for_one, 5, 10},
        lists:map(fun(S) -> child_spec(Account, S) end, Strategies)
    } }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

child_spec(Account, StrategyModule) when is_atom(StrategyModule) ->
    Options = [{account, Account}],
    StartFunc = {StrategyModule, start_link, Options},
    {StrategyModule, StartFunc, permanent, 60000, worker, [StrategyModule]};

child_spec(Account, {StrategyName, StrategyModule}) ->
    Options = [{name, StrategyName}, {account, Account}],
    StartFunc = {StrategyModule, start_link, Options},
    {StrategyName, StartFunc, permanent, 60000, worker, [StrategyModule]};

child_spec(Account, {StrategyName, StrategyModule, StrategyOptions}) ->
    Options = [{name, StrategyName}, {account, Account}] ++ StrategyOptions,
    StartFunc = {StrategyModule, start_link, Options},
    {StrategyName , StartFunc, permanent, 60000, worker, [StrategyModule]}.
