-module(test_strategy).
-behaviour(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% gen_server callbacks:
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% api:
-export([start_link/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {name, account, symbol, period, depth}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Name, Account, Options) ->
    gen_server:start_link(?MODULE, {Name, Account, Options}, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init({Name, Account, Options}) ->
    Depth  = proplists:get_value(depth, Options),
    Symbol = proplists:get_value(symbol, Options),
    Period = proplists:get_value(period, Options),
    Update = proplists:get_value(update, Options),
    timer:apply_interval(Update*1000, gen_server, cast, [self(), update]),
    lager:info("Strategy '~p': started", [Name]),
    {ok, #state{name=Name, account=Account, symbol=Symbol, period=Period, depth=Depth}}.

handle_call({stop, Reason}, _, State) ->
    {stop, Reason, ok, State};

handle_call(_, _, State) ->
    {noreply, ok, State}.

handle_cast(update, State=#state{name=Name, account=Account, symbol=Symbol, period=Period, depth=Depth}) ->
    lager:info("Strategy '~p': updating...", [Name]),
    Data = trade_terminal:get_history(Account, 1, Symbol, Period, Depth),
    lager:info("Strategy '~p': done [~B bars]", [Name, length(Data)]),
    {noreply, State};

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
