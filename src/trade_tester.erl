-module(trade_tester).
-compile(export_all).

-include_lib("trade_periods.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(stats, {pl=[]}).
-record(state, {strategy, strategy_state, terminal_state, stats=#stats{}, history=[], future=[], avg_price, secid}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(TestOptions, Strategy, StrategyOptions) ->
    From     = proplists:get_value(from, TestOptions),
    To       = proplists:get_value(to,   TestOptions),
    Symbol   = proplists:get_value(symbol, TestOptions),
    Period   = proplists:get_value(period, TestOptions),
    Terminal = trade_terminal_state:new(TestOptions),
    State = #state{
        secid          = trade_terminal_state:get_security_id(Symbol, Terminal),
        avg_price      = 0,
        history        = [],
        future         = get_history(Symbol, Period, From, To),
        strategy       = Strategy,
        strategy_state = Strategy:start(Terminal, StrategyOptions),
        terminal_state = Terminal
    },
    test_loop(State).

test_loop(State=#state{future=[]}) ->
    Stats         = State#state.stats,
    Strategy      = State#state.strategy,
    TerminalState = State#state.terminal_state,
    StrategyState = State#state.strategy_state,
    Strategy:stop(TerminalState, StrategyState),
    NewStats = Stats#stats{pl = lists:reverse(Stats#stats.pl)},
    print_report(NewStats),
    NewStats;

test_loop(State=#state{history=History, future=[Bar|Future]}) ->
    Strategy      = State#state.strategy,
    StrategyState = State#state.strategy_state,
    TerminalState = State#state.terminal_state,
    NewHistory    = [Bar|History],
    NewState1     = State#state{history=NewHistory, future=Future},
    NewState2     = handle_signal( Strategy:update(NewHistory, TerminalState, StrategyState), NewState1 ),
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
    Price = trade_utils:close(Bar),
    case trade_terminal_state:get_money(Terminal) of
        Money when Money <  Lots * Price -> exit(no_money);
        Money when Money >= Lots * Price ->
            OldLots = trade_terminal_state:get_position_lots(SecID, Terminal),
            NewAvgPrice = (OldLots*AvgPrice + Lots*Price) / (OldLots+Lots),
            lager:debug("tester: ~s: buy  ~p at ~p, average price: ~p", [trade_utils:to_datetimestr(trade_utils:time(Bar)), Lots, Price, NewAvgPrice]),
            NewTerminal1 = trade_terminal_state:add_position_lots(Lots, SecID, Terminal),
            NewTerminal2 = trade_terminal_state:del_money(Lots*Price, NewTerminal1),
            State#state{avg_price=NewAvgPrice, terminal_state=NewTerminal2}
    end.

sell(Lots, State=#state{secid=SecID, avg_price=AvgPrice, terminal_state=Terminal, stats=Stats}) ->
    Bar = hd(State#state.history),
    Price = trade_utils:close(Bar),
    lager:debug("tester: ~s: sell ~p at ~p, profit: ~p", [trade_utils:to_datetimestr(trade_utils:time(Bar)), Lots, Price, (Lots*Price)-(Lots*AvgPrice)]),
    NewInfo = {(Lots*Price)/(Lots*AvgPrice), (Lots*Price)-(Lots*AvgPrice)},
    NewStats = Stats#stats{pl=[NewInfo|Stats#stats.pl]},
    NewTerminal1 = trade_terminal_state:del_position_lots(Lots, SecID, Terminal),
    NewTerminal2 = trade_terminal_state:add_money(Lots*Price, NewTerminal1),
    State#state{terminal_state=NewTerminal2, stats=NewStats}.

print_report(#stats{pl=PL}) ->
    PL2 = element(1, lists:unzip(PL)),
    lager:info("Geometric mean: ~p~n", [trade_utils:geometric_mean(PL2)]),
    lager:info("Total bids: ~p~n", [length(PL2)]),
    lager:info("Win bids: ~p~n", [length(lists:filter(fun(X) -> X > 1 end, PL2))]),
    lager:info("Percent of wins: ~.2f%~n", [length(lists:filter(fun(X) -> X > 1 end, PL2))/length(PL2)*100])
    .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_history(Symbol, Period, From, undefined) ->
    trade_history:get_history(Symbol, Period, From);

get_history(Symbol, Period, From, To) ->
    trade_history:get_history(Symbol, Period, From, To).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_threaded(Options, Threads) ->
    run_jobs(lists:map(fun make_job/1, Options), Threads).

make_job({Symbol, Period, From, Strategy, Options, TestOptions}) ->
    fun() -> trade_tester:start(Symbol, Period, From, Strategy, Options, TestOptions) end.

run_jobs(Jobs, Threads) when is_integer(Threads) ->
    Workers = spawn_workers(Threads),
    Res = run_jobs(Jobs, Workers),
    stop_workers(Workers),
    Res;

run_jobs(Jobs, Workers) when is_list(Workers) ->
    run_jobs_loop(Jobs, queue:from_list(Workers), [], []).

run_jobs_loop([], _, [], Acc) -> lists:reverse(Acc);
run_jobs_loop([], Idle, Busy, Acc) -> wait_for_result([], Idle, Busy, Acc);

run_jobs_loop(Jobs, Idle, Busy, Acc) ->
    case queue:out(Idle) of
        {empty, _} ->
            wait_for_result(Jobs, Idle, Busy, Acc);
        {{value, Pid}, NewIdle} ->
            Pid ! {job, self(), hd(Jobs)},
            run_jobs_loop(tl(Jobs), NewIdle, [Pid|Busy], Acc)
    end.

wait_for_result(Jobs, Idle, Busy, Acc) ->
    receive
        {job, Pid, Res} ->
            run_jobs_loop(Jobs, queue:in(Pid, Idle), lists:delete(Pid, Busy), [Res|Acc])
    end.

spawn_workers(N) ->
    [ spawn_link( fun worker/0 ) || _ <- lists:seq(1, N) ].

stop_workers(Workers) ->
    [ Pid ! stop || Pid <- Workers ].


worker() ->
    receive
        {job, Pid, Job} ->
            Pid ! {job, self(), catch Job()},
            worker();
        stop ->
            ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


