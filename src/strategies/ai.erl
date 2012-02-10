-module(ai).
-behaviour(bar_strategy).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start/2, stop/2, update/3]).

-define(COMISSION, 0.0003).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {
    lots,
    ranks,
    rank_range,
    rank_history=queue:new(),
    min_period,
    max_period,
    old_signals,
    new_signals
}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Terminal, Options) ->
    MinPeriod = proplists:get_value(min_period, Options, 2),
    MaxPeriod = proplists:get_value(max_period, Options, 100),

    case MaxPeriod < MinPeriod orelse MinPeriod < 2 of
        true  -> exit(invalid_args);
        false -> ok
    end,

    State = #state{
        lots        = 0,
        min_period  = MinPeriod,
        max_period  = MaxPeriod,
        rank_range  = proplists:get_value(rank_range, Options),
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
    State3 = update_lots(State2),
    {get_lots(State3), State3}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

stop(_, _) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_ranks(History, State=#state{old_signals=OldSignals, new_signals=NewSignals, ranks=Ranks}) ->
    Time = trade_utils:time(hd(History)),
    UpdateFn = fun(X) -> update_rank(History, X) end,
    {NewRanks, Deltas} = lists:unzip(lists:map(UpdateFn, lists:zip3(OldSignals, NewSignals, Ranks))),
    NewState1 = State#state{ranks=NewRanks},
    NewState2 = forget_rank_history(Time, NewState1),
    NewState3 = insert_rank_history(Time, Deltas, NewState2),
    NewState3.

update_rank([Bar1,Bar2|_], {_, NewSignal, Rank}) when NewSignal > 0 ->
    Delta = (trade_utils:close(Bar1) / trade_utils:close(Bar2)),
    lager:debug("ai: ~s: updating delta ~p -> ~p", [trade_utils:to_datetimestr(trade_utils:time(Bar1)), Rank, Rank*Delta]),
    {Rank * Delta, Delta};

update_rank([_Bar1|_], {OldSignal, _, Rank}) when OldSignal > 0 ->
    Delta = ((1-?COMISSION) / (1+?COMISSION)),
    lager:debug("ai: ~s: updating delta ~p -> ~p", [trade_utils:to_datetimestr(trade_utils:time(_Bar1)), Rank, Rank*Delta]),
    {Rank * Delta, Delta};

update_rank(_, {_, _, Rank}) ->
    {Rank, 1}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

forget_rank_history(Time, State=#state{ranks=Ranks, rank_history=History}) ->
    case queue:out(History) of
        {{value, {ForgetAt, Deltas}}, NewHistory} when ForgetAt =< Time ->
            NewRanks = lists:map(
                fun({X,Y}) ->
                    lager:debug("ai: ~s: reverting delta ~p -> ~p", [trade_utils:to_datetimestr(Time), X, X/Y]),
                    X/Y
                end,
                lists:zip(Ranks, Deltas)),
            NewState = State#state{ranks=NewRanks, rank_history=NewHistory},
            NewState;
        _ ->
            State
    end.

insert_rank_history(_, _, State=#state{rank_range=undefined}) -> State;
insert_rank_history(Time, Deltas, State=#state{rank_history=History, rank_range=Range}) ->
    ForgetTime = time_shift(Time, Range),
    State#state{rank_history=queue:in({ForgetTime, Deltas}, History)}.

time_shift(Time, {N,    day}) -> trade_utils:add_days(Time, N);
time_shift(Time, {N,   days}) -> trade_utils:add_days(Time, N);
time_shift(Time, {N,   week}) -> trade_utils:add_days(Time, N*7);
time_shift(Time, {N,  weeks}) -> trade_utils:add_days(Time, N*7);
time_shift(Time, {N,  month}) -> trade_utils:add_days(Time, N*30);
time_shift(Time, {N, months}) -> trade_utils:add_days(Time, N*30);
time_shift(Time, {N,   year}) -> trade_utils:add_days(Time, N*365);
time_shift(Time, {N,  years}) -> trade_utils:add_days(Time, N*365).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_signals(History, State=#state{min_period=Min, max_period=Max}) ->
    Averages = averages(trade_utils:close(lists:sublist(History, Max))),
    BaseMA = element(2, lists:keyfind(5,1,Averages)),
    NewSignals = signals(lists:sublist(Averages, Min, Max), BaseMA),
    State#state{old_signals=State#state.new_signals, new_signals=NewSignals}.

signals(Averages, BaseMA) ->
    Fun = fun({_,Avg},Acc) -> [update_signal(Avg, hd(Acc), BaseMA)|Acc] end,
    tl(lists:reverse( lists:foldl(Fun, [0], Averages) )).

update_signal(Average, LastAverage, BaseMA) ->
    LastAverage + BaseMA - Average.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_lots(State=#state{}) ->
    NewPeriod = get_best_period(State),
    NewSignal = get_signal(NewPeriod, State),
    State#state{lots=update_lots(NewSignal, State)}.

update_lots(Sig, _) when Sig  > 0 -> 1;
update_lots(Sig, _) when Sig =< 0 -> 0.

get_lots(#state{lots=Lots}) -> Lots.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


get_best_period(#state{min_period=Min, ranks=Ranks}) ->
    get_best_period(Ranks, 0, Min, Min).

get_best_period([], _, _, Period) -> Period;
get_best_period([CurRank|Tail], MaxRank, CurPeriod, MaxPeriod) ->
    case CurRank > MaxRank of
        true  -> get_best_period(Tail, CurRank, CurPeriod+1, CurPeriod);
        false -> get_best_period(Tail, MaxRank, CurPeriod+1, MaxPeriod)
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

run() ->
    Symbol = "SBER",
    Period = 60,
    run(Symbol, Period).

run(Symbol, Period) ->
    trade_tester:test
    (
        [
            {from, {2012, 1, 1}},
            {to,   {2012, 2, 1}},
            {symbol, Symbol},
            {period, Period},
            {money, 100000}
        ],
        ai,
        [
            {rank_range, {1, year}},
            {min_period, 10},
            {max_period, 10},
%             {learn_period, {{2010, 1, 1}, {2011, 1, 1}}},
            {security, Symbol},
            {period, Period}
        ]
    ).
