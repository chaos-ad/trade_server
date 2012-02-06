-module(ai).
-behaviour(bar_strategy).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start/2, stop/2, update/3]).

-define(COMISSION, 0.0003).
% -define(COMISSION, 0.1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {min_period, max_period, ranks, old_signals, new_signals}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Terminal, Options) ->
    MinPeriod = proplists:get_value(min_period, Options, 2),
    MaxPeriod = proplists:get_value(max_period, Options, 100),

    case MaxPeriod < MinPeriod orelse MinPeriod < 2 of
        true  -> exit(invalid_args);
        false -> ok
    end,

    State = #state{
        min_period  = MinPeriod,
        max_period  = MaxPeriod,
        ranks       = lists:duplicate(MaxPeriod-MinPeriod+1, 1),
        old_signals = lists:duplicate(MaxPeriod-MinPeriod+1, 0),
        new_signals = lists:duplicate(MaxPeriod-MinPeriod+1, 0)
    },

    Learn = proplists:get_value(learn_period, Options),
    Period = proplists:get_value(period, Options),
    Security = proplists:get_value(security, Options),

    case Learn of
        undefined  -> State;
        {From, To} ->
            lager:debug("ai: Training using historical data in range [~s : ~s]...", [edate:date_to_string(From), edate:date_to_string(To)]),
            NewState = trade_tester:learn(Security, Period, From, To, ?MODULE, State, Terminal),
            lager:debug("ai: Training complete"),
            NewState;
        From ->
            lager:debug("ai: Training using historical data from ~s...", [edate:date_to_string(From)]),
            NewState = trade_tester:learn(Security, Period, From, ?MODULE, State, Terminal),
            lager:debug("ai: Training complete"),
            NewState
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Нет сделок:
update(History, _Terminal, State=#state{max_period=MaxPeriod}) when length(History) < MaxPeriod ->
    {0, State};

update(History, _Terminal, State0) ->

    State1 = update_ranks(History, State0),
    State2 = update_signals(History, State1),

    OldPeriod = get_best_period(State0),
    NewPeriod = get_best_period(State2),

    OldSignal = get_signal(OldPeriod, State0),
    NewSignal = get_signal(NewPeriod, State2),

    OldRank = get_rank(OldPeriod, State0),
    NewRank = get_rank(NewPeriod, State2),

    Lots = get_lots(NewSignal, State2),

    Args = [trade_utils:time_str(hd(History)), OldPeriod, NewPeriod, OldSignal, OldRank, NewSignal, NewRank, Lots],
    lager:debug("ai: ~s: periods: ~p -> ~p; signals: ~p [rank: ~p] -> ~p [rank: ~p]; lots: ~p", Args),

    {Lots, State2}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_lots(Sig, _) when Sig  > 0 -> 1;
get_lots(Sig, _) when Sig =< 0 -> 0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

stop(_, _) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_ranks(History, State=#state{old_signals=OldSignals, new_signals=NewSignals, ranks=Ranks}) ->
    NewRanks = lists:map(fun(X) -> update_rank(History, X) end, lists:zip3(OldSignals, NewSignals, Ranks)),
    State#state{ranks=NewRanks}.

update_rank([Bar1,Bar2|_], {_, NewSignal, Rank}) when NewSignal > 0 ->
    Rank * (trade_utils:close(Bar1) / trade_utils:close(Bar2));

update_rank(_, {OldSignal, _, Rank}) when OldSignal > 0 ->
    Rank * ((1-?COMISSION) / (1+?COMISSION));

update_rank(_, {_, _, Rank}) ->
    Rank.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_signals(History, State=#state{min_period=Min, max_period=Max}) ->
    Averages = averages(trade_utils:close(lists:sublist(History, Max))),
    BaseMA = element(2, lists:keyfind(5,1,Averages)),
    NewSignals = signals(lists:sublist(Averages, Min, Max), BaseMA),
    State#state{old_signals=State#state.new_signals, new_signals=NewSignals}.

signals(Averages, BaseMA) ->
    Fun = fun({_,Avg},Acc) -> [update_signal(Avg, hd(Acc), BaseMA)|Acc] end,
    Signals1 = lists:foldl(Fun, [0], Averages),
    Signals2 = tl(lists:reverse(Signals1)),
    Signals2.

update_signal(Average, LastAverage, BaseMA) ->
    LastAverage + BaseMA - Average.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% TODO: Считать лучший период сразу при пересчёте сигналов
get_best_period(#state{min_period=Min, new_signals=Signals}) ->
    get_best_period(Signals, Min, 0, Min).

get_best_period([], _, _, Period) -> Period;
get_best_period([CurSignal|Tail], CurPeriod, MaxSignal, MaxPeriod) ->
    case CurSignal > MaxSignal of
        true  -> get_best_period(Tail, CurPeriod+1, CurSignal, CurPeriod);
        false -> get_best_period(Tail, CurPeriod+1, MaxSignal, MaxPeriod)
    end.

get_signal(Period, #state{min_period=Min, new_signals=Signals}) ->
    lists:nth(Period-Min+1, Signals).

get_rank(Period, #state{min_period=Min, ranks=Ranks}) ->
    lists:nth(Period-Min+1, Ranks).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

averages(List) ->
    averages(List, 1, 0, 0, 0, []).

averages([], _, _, _, _, Acc) -> lists:reverse(Acc);
averages([Value|Tail], Period, ValSum, TotalSum, PeriodSum, Acc) ->
    NewPeriod = Period + 1,
    NewValSum = ValSum + Value,
    NewTotalSum = TotalSum + NewValSum,
    NewPeriodSum = PeriodSum + Period,
    NewAcc = [{Period, NewTotalSum / NewPeriodSum}|Acc],
    averages(Tail, NewPeriod, NewValSum, NewTotalSum, NewPeriodSum, NewAcc).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% % TODO: тупо проёбываем на SPFB.GOLD
run() ->
    Symbol = "SBER",
    Period = 15,
    trade_tester:test
    (
        [
            {from, {2010, 1, 1}},
            {to,   {2012, 1, 1}},
            {symbol, Symbol},
            {period, Period},
            {money, 100000}
        ],
        ai,
        [
            {min_period, 5},
            {max_period, 500},
            {learn_period, {{2009, 1, 1}, {2010, 1, 1}}},
            {security, Symbol},
            {period, Period}
        ]
    ).
