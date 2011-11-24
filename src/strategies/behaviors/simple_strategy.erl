-module(simple_strategy).
-behaviour(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% gen_server callbacks:
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([behaviour_info/1]).

behaviour_info(callbacks) -> [{start, 1}, {stop, 1}, {update, 2}];
behaviour_info(_) -> undefined.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {name, module, symbol, period, account, custom_state}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Module, Options) ->
    gen_server:start_link(?MODULE, {Module, Options}, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init({Module, Options}) ->
    process_flag(trap_exit, true),
    Name    = proplists:get_value(name, Options),
    Account = proplists:get_value(account, Options),

    Symbol  = proplists:get_value(symbol, Options),
    Period  = proplists:get_value(period, Options),

    trade_terminal:subscribe(Account, quotes, [{1, Symbol}]),

    StrategyState = Module:start(Options),
    lager:info("Strategy '~p': started", [Name]),
    {ok, #state{name=Name, module=Module, symbol=Symbol, period=Period, account=Account, custom_state=StrategyState}}.

handle_call(_, _, State) ->
    {noreply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

% handle_info({new_quotes, _}, State=#state{}) ->
%     

handle_info(_, State) ->
    {noreply, State}.

terminate(_, #state{name=Name, module=Module, custom_state=CustomState}) ->
    Module:stop(CustomState),
    lager:info("Strategy '~p': stopped", [Name]).

code_change(_, State, _) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
