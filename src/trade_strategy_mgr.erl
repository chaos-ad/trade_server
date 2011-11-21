-module(trade_strategy_mgr).
-behaviour(supervisor).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start_link/2, init/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(CHILD(I), {I, {I, start_link, []}, permanent, 60000, worker, [I]}).
-define(CHILD(I, Args), {I, {I, start_link, Args}, permanent, 60000, worker, [I]}).
-define(CHILD(I, Args, Role), {I, {I, start_link, Args}, permanent, 60000, Role, [I]}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Name, Strategies) ->
    supervisor:start_link(?MODULE, {Name, Strategies}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init({Name, Strategies}) ->
    {ok, { {one_for_one, 5, 10},
        lists:map(fun(S) -> child_spec(Name, S) end, Strategies)
    } }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

child_spec(Account, {StrategyName, StrategyModule, Options}) ->
    StartFunc = {StrategyModule, start_link, [StrategyName, Account, Options]},
    {StrategyName , StartFunc, permanent, 60000, worker, [StrategyModule]};

child_spec(Account, {StrategyName, StrategyModule}) ->
    StartFunc = {StrategyModule, start_link, [StrategyName, Account]},
    {StrategyName, StartFunc, permanent, 60000, worker, [StrategyModule]}.
