-module(bar_strategy).
-behaviour(trade_strategy).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start/3, stop/1, handle_cast/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {name, terminal, strategy, strategy_state, security, period, depth, last_bar}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Name, Terminal, Options) ->
    {Strategy, StrategyOptions} = proplists:get_value(strategy, Options),
    State = #state{
        name            = Name,
        terminal        = Terminal,
        strategy        = Strategy,
        strategy_state  = Strategy:start(StrategyOptions),
        security        = proplists:get_value(security, Options),
        period          = proplists:get_value(period, Options),
        depth           = proplists:get_value(depth, Options, 1000)
    },
    UpdateInterval = proplists:get_value(update_interval, Options, 60),
    timer:apply_interval(UpdateInterval * 1000, gen_server, cast, [self(), update]),
    {ok, State}.

handle_cast(update, State=#state{terminal=Terminal, security=Security, period=Period, depth=Depth, last_bar=LastBar}) ->
    Strategy = State#state.strategy,
    StrategyState = State#state.strategy_state,
    TerminalState = trade_terminal:get_state(State#state.terminal),
    case trade_terminal:get_history(Terminal, Security, Period, 1, true) of
        {ok, [LastBar]} -> {noreply, State};
        {ok,  [NewBar]} ->
            NewState = State#state{last_bar=NewBar},
            {ok, History} = trade_terminal:get_history(Terminal, Security, Period, Depth, true),
            {noreply, handle_signal( Strategy:update(History, TerminalState, StrategyState), NewState )}
    end;

handle_cast(_, State) ->
    {noreply, State}.

stop(_) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_signal({Signal, StrategyState}, State) ->
    handle_signal(Signal, State#state{strategy_state=StrategyState});

handle_signal(NewLots, State=#state{terminal=Terminal, security=Security}) ->
    case trade_terminal:get_position_lots(Terminal, Security) of
        Lots when Lots  <  NewLots -> buy(NewLots-Lots, State);
        Lots when Lots  >  NewLots -> sell(Lots-NewLots, State);
        Lots when Lots =:= NewLots -> State
    end.

buy(Lots, State=#state{terminal=Terminal, security=Security}) ->
    lager:info("strategy ~p: buying  ~p lots of ~p...", [?MODULE, Lots, Security]),
    Result = trade_terminal:buy_order(Terminal, Security, Lots),
    lager:info("strategy ~p: buying  ~p lots of ~p: ~p", [?MODULE, Lots, Security, Result]),
    State.

sell(Lots, State=#state{terminal=Terminal, security=Security}) ->
    lager:info("strategy ~p: selling ~p lots of ~p...", [?MODULE, Lots, Security]),
    Result = trade_terminal:sell_order(Terminal, Security, Lots),
    lager:info("strategy ~p: selling ~p lots of ~p: ~p", [?MODULE, Lots, Security, Result]),
    State.
