
-module(trade_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I), {I, {I, start_link, []}, permanent, 60000, worker, [I]}).
-define(CHILD(I, Options), {I, {I, start_link, Options}, permanent, 60000, worker, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, [
        ?CHILD(trade_db),
        ?CHILD(trade_history),
        ?CHILD(trade_terminal_manager),
        ?CHILD(trade_strategy_manager)
    ]} }.

