-module(bar_strategy).
-behaviour(base_strategy).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start/3, stop/1, handle_cast/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {name, terminal, strategy, strategy_state, security, period, depth, last_bar}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Name, Terminal, Options) ->
    lager:info("bar_strategy options: ~p", [Options]),
    {Strategy, StrategyOptions} = proplists:get_value(strategy, Options),
    lager:info("bar_strategy strategy options: ~p", [StrategyOptions]),
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

handle_cast(update, State=#state{name=Name, security=Security, period=Period, depth=Depth, last_bar=LastBar}) ->
    lager:debug("Strategy '~p': updating...", [Name]),
    Strategy = State#state.strategy,
    StrategyState = State#state.strategy_state,
    TerminalState = trade_terminal:get_state(State#state.terminal),
    case hd(trade_terminal:get_history(Security, Period, 1, true)) =:= LastBar of
        true  -> State;
        false ->
            lager:debug("Strategy '~p': really updating...", [Name]),
            History = trade_terminal:get_history(Security, Period, Depth, true),
            handle_signal( Strategy:update(History, TerminalState, StrategyState), State )
    end;

handle_cast(_, State) ->
    {noreply, State}.

stop(_) ->
    ok.

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
