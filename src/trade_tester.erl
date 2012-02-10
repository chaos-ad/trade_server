-module(trade_tester).
-compile(export_all).

-include_lib("trade_periods.hrl").

-define(COMISSION, 0.0003).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(stats, {pl=[], first_bar, last_bar}).
-record(state, {strategy, strategy_state, terminal_state, stats=#stats{}, history=[], future=[], avg_price, secid}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

learn(Symbol, Period, From, Strategy, StrategyState, Terminal) ->
    learn(Symbol, Period, From, undefined, Strategy, StrategyState, Terminal).

learn(Symbol, Period, From, To, Strategy, StrategyState, Terminal) ->
    State = #state{
        history        = [],
        future         = get_history(Symbol, Period, From, To),
        strategy       = Strategy,
        strategy_state = StrategyState,
        terminal_state = Terminal
    },
    learn_loop(State).

learn_loop(#state{future=[], strategy_state=SState}) -> SState;
learn_loop(State=#state{strategy=Strategy, history=History, future=[Bar|Future]}) ->
    NewHistory    = [Bar|History],
    TState        = State#state.terminal_state,
    SState        = State#state.strategy_state,
    {_, SState1}  = Strategy:update(NewHistory, TState, SState),
    learn_loop(State#state{history=NewHistory, future=Future, strategy_state=SState1}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(TestOptions, Strategy, StrategyOptions) ->
    From     = proplists:get_value(from, TestOptions),
    To       = proplists:get_value(to,   TestOptions),
    Symbol   = proplists:get_value(symbol, TestOptions),
    Period   = proplists:get_value(period, TestOptions),
    Terminal = trade_terminal_state:new(TestOptions),
    History  = get_history(Symbol, Period, From, To),
    State = #state{
        secid          = trade_terminal_state:get_security_id(Symbol, Terminal),
        avg_price      = 0,
        history        = [],
        future         = History,
        strategy       = Strategy,
        terminal_state = Terminal,
        strategy_state = Strategy:start(Terminal, StrategyOptions),
        stats          = #stats{first_bar=hd(History), last_bar=lists:nth(length(History), History)}
    },
    test_loop(State).

test_loop(State=#state{strategy=Strategy, future=[]}) ->
    NewState      = handle_signal( {0, State#state.strategy_state}, State ),
    Stats         = NewState#state.stats,
    TState        = State#state.terminal_state,
    SState        = State#state.strategy_state,
    Strategy:stop(TState, SState),
    NewStats = Stats#stats{pl = lists:reverse(Stats#stats.pl)},
    print_report(NewStats),
    NewStats;

test_loop(State=#state{strategy=Strategy, history=History, future=[Bar|Future]}) ->
    NewHistory    = [Bar|History],
    TState        = State#state.terminal_state,
    SState        = State#state.strategy_state,
    NewState1     = State#state{history=NewHistory, future=Future},
    NewState2     = handle_signal( Strategy:update(NewHistory, TState, SState), NewState1 ),
    test_loop(NewState2).

handle_signal({Signal, NewStrategyState}, State) ->
    handle_signal(Signal, State#state{strategy_state=NewStrategyState});

handle_signal(NewLots, State=#state{secid=SecID, terminal_state=Terminal}) ->
    case trade_terminal_state:get_position_lots(SecID, Terminal) of
        Lots when Lots  <  NewLots -> buy(NewLots-Lots, State);
        Lots when Lots  >  NewLots -> sell(Lots-NewLots, State);
        Lots when Lots =:= NewLots -> State
    end.

buy(Lots, State=#state{secid=SecID, avg_price=AvgPrice, terminal_state=Terminal}) ->
    Bar = hd(State#state.history),
    Price = trade_utils:close(Bar) * (1 + ?COMISSION),
    case trade_terminal_state:get_money(Terminal) of
        Money when Money <  Lots * Price -> exit(no_money);
        Money when Money >= Lots * Price ->
            OldLots = trade_terminal_state:get_position_lots(SecID, Terminal),
            NewAvgPrice = (OldLots*AvgPrice + Lots*Price) / (OldLots+Lots),
            lager:debug("tester: ~s:  buy ~p lots: ~.4f", [trade_utils:time_str(Bar), Lots, Lots*Price]),
            NewTerminal1 = trade_terminal_state:add_position_lots(Lots, SecID, Terminal),
            NewTerminal2 = trade_terminal_state:del_money(Lots*Price, NewTerminal1),
            State#state{avg_price=NewAvgPrice, terminal_state=NewTerminal2}
    end.

sell(Lots, State=#state{secid=SecID, avg_price=AvgPrice, terminal_state=Terminal, stats=Stats}) ->
    Bar = hd(State#state.history),
    Price = trade_utils:close(Bar) * (1 - ?COMISSION),
    RelProfit = (Lots*Price)/(Lots*AvgPrice),
    AbsProfit = (Lots*Price)-(Lots*AvgPrice),
    NewStats = Stats#stats{pl=[RelProfit|Stats#stats.pl]},
    NewTerminal1 = trade_terminal_state:del_position_lots(Lots, SecID, Terminal),
    NewTerminal2 = trade_terminal_state:add_money(Lots*Price, NewTerminal1),
    NewMoney = trade_terminal_state:get_money(NewTerminal2),

    Args = [trade_utils:time_str(Bar), Lots, (Lots*Price), AbsProfit, (RelProfit-1)/100, NewMoney],
    lager:debug("tester: ~s: sell ~p lots: ~.4f (profit: ~.4f (~.4f%), new equity: ~.4f)", Args),
    State#state{terminal_state=NewTerminal2, stats=NewStats}.

print_report(#stats{pl=PL, first_bar=First, last_bar=Last}) ->
    Bids = length(PL),
    Wons = length(lists:filter(fun(X) -> X > 1 end, PL)),
    lager:info("Win bids: ~p~n", [Wons]),
    lager:info("Total bids: ~p~n", [Bids]),
    lager:info("Percent of wins: ~.2f%~n", [percent(Wons, Bids)]),
    lager:info("Geometric mean: ~p~n", [trade_utils:geometric_mean(PL)]),
    lager:info("Arithmetic mean: ~p~n", [trade_utils:arithmetic_mean(PL)]),
    lager:info("Variance: ~p~n", [trade_utils:variance(PL)]),
    lager:info("Standard derivation: ~p~n", [trade_utils:standard_derivation(PL)]),
    lager:info("Total wealth relative: ~p~n", [trade_utils:multiply(PL)]),
    lager:info("Buy&Hold wealth relative: ~p~n", [trade_utils:close(Last)/trade_utils:open(First)])
    .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_history(Symbol, Period, From, undefined) ->
    trade_history:get_history(Symbol, Period, From);

get_history(Symbol, Period, From, To) ->
    trade_history:get_history(Symbol, Period, From, To).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

percent(_, 0) -> 0.0;
percent(X, Y) -> X/Y*100.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

