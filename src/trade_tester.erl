-module(trade_tester).
-compile(export_all).

-include_lib("trade_periods.hrl").

-define(COMISSION, 0.0003).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {
    strategy,
    strategy_state,
    terminal_state,
    stats,
    history=[],
    future=[],
    secid
}).

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
    Money    = trade_terminal_state:get_money(Terminal),
    History  = get_history(Symbol, Period, From, To),
    State = #state{
        secid          = trade_terminal_state:get_security_id(Symbol, Terminal),
        history        = [],
        future         = History,
        strategy       = Strategy,
        terminal_state = Terminal,
        strategy_state = Strategy:start(Terminal, StrategyOptions),
        stats          = trade_stats:set_money(Money, trade_stats:new())
    },
    test_loop(State).

test_loop(State=#state{strategy=Strategy, future=[]}) ->
    Signal = {0, State#state.strategy_state},
    NewState = handle_signal(Signal, State),
    Strategy:stop(State#state.terminal_state, State#state.strategy_state),
    Report = trade_stats:get_report(NewState#state.stats),
    trade_stats:print_report(Report),
    Report;

test_loop(State=#state{strategy=Strategy, history=History, future=[Bar|Future]}) ->
    NewHistory = [Bar|History],
    NewStats = trade_stats:add_bar(Bar, State#state.stats),
    NewSignal = Strategy:update(NewHistory, State#state.terminal_state, State#state.strategy_state),
    NewState = handle_signal(NewSignal, State#state{history=NewHistory, future=Future, stats=NewStats}),
    test_loop(NewState).

handle_signal({Signal, NewStrategyState}, State) ->
    handle_signal(Signal, State#state{strategy_state=NewStrategyState});

handle_signal(NewLots, State=#state{secid=SecID, terminal_state=Terminal}) ->
    case trade_terminal_state:get_position_lots(SecID, Terminal) of
        Lots when Lots  <  NewLots -> buy(NewLots-Lots, State);
        Lots when Lots  >  NewLots -> sell(Lots-NewLots, State);
        Lots when Lots =:= NewLots -> State
    end.

buy(Lots, State=#state{secid=SecID, terminal_state=Terminal}) ->
    Bar = hd(State#state.history),
    Price = trade_utils:close(Bar) * (1 + ?COMISSION),
    case trade_terminal_state:get_money(Terminal) of
        Money when Money <  Lots * Price -> exit(no_money);
        Money when Money >= Lots * Price ->
            NewTerminal1 = trade_terminal_state:add_position_lots(Lots, SecID, Terminal),
            NewTerminal2 = trade_terminal_state:del_money(Lots*Price, NewTerminal1),
            NewMoney  = trade_terminal_state:get_money(NewTerminal2),
            NewStats1 = trade_stats:buy(Bar, SecID, Lots, Price, State#state.stats),
            NewStats2 = trade_stats:set_money(NewMoney, NewStats1),
            State#state{terminal_state=NewTerminal2, stats=NewStats2}
    end.

sell(Lots, State=#state{secid=SecID, terminal_state=Terminal}) ->
    Bar = hd(State#state.history),
    Price = trade_utils:close(Bar) * (1 - ?COMISSION),
    NewTerminal1 = trade_terminal_state:del_position_lots(Lots, SecID, Terminal),
    NewTerminal2 = trade_terminal_state:add_money(Lots*Price, NewTerminal1),
    NewMoney  = trade_terminal_state:get_money(NewTerminal2),
    NewStats1 = trade_stats:sell(Bar, SecID, Lots, Price, State#state.stats),
    NewStats2 = trade_stats:set_money(NewMoney, NewStats1),
    State#state{terminal_state=NewTerminal2, stats=NewStats2}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_history(Symbol, Period, From, undefined) ->
    trade_history:get_history(Symbol, Period, From);

get_history(Symbol, Period, From, To) ->
    trade_history:get_history(Symbol, Period, From, To).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

