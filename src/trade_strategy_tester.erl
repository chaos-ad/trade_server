-module(trade_strategy_tester).
-compile(export_all).

-include_lib("trade_periods.hrl").
-include_lib("trade_test_stats.hrl").

test(From, To, Step, Money, Strategy, StrategyOptions) ->
    {ok, TerminalPid} = trade_terminal:start_link(testmode, [{saldo, Money}, {time, From}]),
    {ok, StrategyPid} = Strategy:start_link(TerminalPid, StrategyOptions),
    T1 = trade_utils:to_unixtime(From),
    T2 = trade_utils:to_unixtime(To),
    Result = test_loop(T1, T2, Step*60, TerminalPid, Strategy, StrategyPid),
    Strategy:stop(StrategyPid),
    trade_terminal:stop(TerminalPid),
    Result.

test_loop(T1, T2, Step, TerminalPid, Strategy, StrategyPid) when T1 =< T2 ->
    trade_terminal_test:set_time(TerminalPid, T1),
    Strategy:update(StrategyPid),
    test_loop(T1+Step, T2, Step, TerminalPid, Strategy, StrategyPid);

test_loop(T1, T2, _, TerminalPid, _, _) when T1 > T2 ->
    Stats = trade_terminal_test:get_stats(TerminalPid),
    print_report(Stats).

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
            trade_strategy_tester:test({2009, 3, 2}, {2009, 3, 3}, ?PERIOD_M1, 300000, ma, [{symbol, "GAZP"}, {timeframe, ?PERIOD_M1}])
        end
    ),
    io:format("Done in ~p seconds~n", [TC/1000/1000]).

profile() ->
    fprof:apply(fun test/0, []),
    fprof:profile(),
    fprof:analyse().
