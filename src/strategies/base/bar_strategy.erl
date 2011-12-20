-module(bar_strategy).
-behaviour(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% gen_server callbacks:
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% api:
-export([start_link/2, stop/1, stop/2, update/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {terminal, strategy, strategy_state, security, period, depth, lots, interval, last_bar}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Terminal, Options) ->
    gen_server:start_link(?MODULE, {Terminal, Options}, []).

stop(Pid) ->
    stop(Pid, normal).

stop(Pid, Reason) ->
    gen_server:call(Pid, {stop, Reason}).

update(Pid) ->
    gen_server:cast(Pid, update).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init({Terminal, Options}) ->
    Strategy            = proplists:get_value(strategy,         Options),
    StrategyOptions     = proplists:get_value(strategy_options, Options),
    State = #state{
        terminal        = Terminal,
        strategy        = Strategy,
        strategy_state  = Strategy:start(StrategyOptions),
        security        = proplists:get_value(security, Options),
        period          = proplists:get_value(period, Options),
        depth           = proplists:get_value(depth, Options, 1000),
        interval        = proplists:get_value(interval, Options, 60)
    },
    timer:apply_interval(State#state.interval * 1000, ?MODULE, update, [self()]),
    {ok, State}.

handle_call({stop, Reason}, _, State) ->
    {stop, Reason, ok, State};

handle_call(_, _, State) ->
    {noreply, ok, State}.

handle_cast(update, State=#state{security=Security, period=Period, depth=Depth, last_bar=LastBar}) ->
    Strategy = State#state.strategy,
    StrategyState = State#state.strategy_state,
    TerminalState = trade_terminal:get_state(State#state.terminal),
    case hd(trade_terminal:get_history(Security, Period, 1, true)) =:= LastBar of
        true  -> State;
        false ->
            History = trade_terminal:get_history(Security, Period, Depth, true),
            handle_signal( Strategy:update(History, TerminalState, StrategyState), State )
    end;

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_signal({Signal, StrategyState}, State) ->
    handle_signal(Signal, State#state{strategy_state=StrategyState});

handle_signal(NewLots, State=#state{terminal=Terminal, security=Security}) ->
    case trade_terminal:get_pos_lots(Terminal, Security) of
        Lots when Lots  <  NewLots -> buy(NewLots-Lots, State);
        Lots when Lots  >  NewLots -> sell(Lots-NewLots, State);
        Lots when Lots =:= NewLots -> State
    end.

buy(Lots, State=#state{terminal=Terminal, security=Security}) ->
    Result = trade_terminal:buy_order(Terminal, Security, Lots),
    lager:debug("strategy ~p: buying  ~p lots of ~p: ~p", [?MODULE, Lots, Security, Result]),
    State.

sell(Lots, State=#state{terminal=Terminal, security=Security}) ->
    Result = trade_terminal:sell_order(Terminal, Security, Lots),
    lager:debug("strategy ~p: selling ~p lots of ~p: ~p", [?MODULE, Lots, Security, Result]),
    State.
