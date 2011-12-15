-module(trade_strategy_tester).
-compile(export_all).

-include_lib("trade_periods.hrl").
-include_lib("trade_test_stats.hrl").

test(Symbol, Period, From, Money, Strategy, Options) ->
    UpdateSeries = trade_utils:time(trade_history:get_history(Symbol, Period, From)),
    test_loop(UpdateSeries, Money, Strategy, [{symbol, Symbol}, {period, Period}] ++ Options).

test(Symbol, Period, From, To, Money, Strategy, Options) ->
    UpdateSeries = trade_utils:time(trade_history:get_history(Symbol, Period, From, To)),
    test_loop(UpdateSeries, Money, Strategy, [{symbol, Symbol}, {period, Period}] ++ Options).

test_loop(UpdateSeries, Money, Strategy, Options) ->
    {ok, TerminalPid} = trade_terminal:start_link(testmode, [{saldo, Money}]),
    {ok, StrategyPid} = Strategy:start_link(TerminalPid, Options),
    lists:foreach
    (
        fun(Time) ->
            trade_terminal_test:set_time(TerminalPid, Time),
            Strategy:update(StrategyPid)
        end,
        UpdateSeries
    ),
    Stats = trade_terminal_test:get_stats(TerminalPid),
    print_report(Stats),
    Strategy:stop(StrategyPid),
    trade_terminal:stop(TerminalPid),
    Stats.

print_report(Stats) ->
    io:format("=== Statistics ===~n"),
    lists:foreach(fun print_bid/1, Stats#test_stats.history),
    io:format("==================~n").

print_bid({Dir,Symbol,Lots,Time,Money}) ->
    io:format("~s: ~-4.w ~s ~p ~p~n", [trade_utils:to_datetimestr(Time), Dir, Symbol, Lots, Money]).

test() ->
    {TC, _} =
    timer:tc
    (
        fun() ->
            trade_strategy_tester:test("GAZP", ?PERIOD_H1, {2011, 1, 1}, 10000, ma, [])
        end
    ),
    io:format("Done in ~p seconds~n", [TC/1000/1000]).

profile() ->
    fprof:apply(fun test/0, []),
    fprof:profile(),
    fprof:analyse().
