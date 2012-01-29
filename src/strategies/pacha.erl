-module(pacha).
-behaviour(bar_strategy).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start/2, stop/2, update/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {min_period, max_period, ranks, signals}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Terminal, Options) ->
    MinPeriod = proplists:get_value(min_period, Options, 2),
    MaxPeriod = proplists:get_value(max_period, Options, 100),
    case MaxPeriod < MinPeriod orelse MinPeriod < 2 of
        true  -> exit(invalid_args);
        false -> ok
    end,
    State =
    #state{
        min_period = MinPeriod,
        max_period = MaxPeriod,
        ranks      = [ {X,1} || X <- lists:seq(MinPeriod, MaxPeriod) ],
        signals    = [ {X,0} || X <- lists:seq(MinPeriod, MaxPeriod) ]
    },

    History =
    case proplists:get_value(learn_period, Options) of
        undefined  -> [];
        From ->
            Sec = proplists:get_value(security, Options),
            Period = proplists:get_value(period, Options),
            case From of
                {T1, T2} ->
                    lager:debug("pacha: Training using historical data in range [~s : ~s]...", [edate:date_to_string(T1), edate:date_to_string(T2)]),
                    trade_history:get_history(Sec, Period, T1, T2);
                T1 when is_tuple(T1) ->
                    lager:debug("pacha: Training using historical data from ~s...", [edate:date_to_string(T1)]),
                    trade_history:get_history(Sec, Period, T1)
            end
    end,
    NewState = learn([], History, Terminal, State),
    lager:debug("pacha: Training complete"),
    NewState.

learn(_, [], _, State) -> State;
learn(History, [Bar|Future], Terminal, State) ->
    NewHistory = [Bar|History],
    {_, NewState} = update(NewHistory, Terminal, State),
    learn(NewHistory, Future, Terminal, NewState).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Нет сделок:
update(History, _Terminal, State=#state{max_period=MaxPeriod}) when length(History) < MaxPeriod ->
    {0, State};

update(History, _Terminal, State=#state{signals=Signals, ranks=Ranks}) ->

    NewRanks   = update_ranks(History, Signals, Ranks),
    NewSignals = update_signals(History, State#state.min_period, State#state.max_period),

%     {CurPeriod, _CurRank}   = hd(lists:reverse(lists:keysort(2, Ranks))),
%     {CurPeriod, CurSignal} = lists:keyfind(CurPeriod, 1, Signals),

    {NewPeriod, _NewRank}   = hd(lists:reverse(lists:keysort(2, NewRanks))),
    {NewPeriod, NewSignal} = lists:keyfind(NewPeriod, 1, NewSignals),

%     Time = trade_utils:to_datetimestr(trade_utils:time(hd(History))),
%     Args = [Time, CurPeriod,NewPeriod,CurSignal,NewSignal,_CurRank,_NewRank],
%     lager:debug("pacha: ~s: period ~p -> ~p; signal: ~p -> ~p; rank ~p -> ~p", Args),

    {lots(NewSignal), State#state{signals=NewSignals, ranks=NewRanks}}.

lots(Sig) when Sig  > 0 -> 1;
lots(Sig) when Sig =< 0 -> 0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

stop(_, #state{ranks=Ranks}) ->
    lager:debug("pacha: final ranks:~n"),
    lists:foreach( fun({Period, Rank}) -> lager:debug("pacha: ~p: ~p~n", [Period,Rank]) end, lists:keysort(2, Ranks) ),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_signals(History, MinPeriod, MaxPeriod) ->
    SignalsRaw = lists:foldl
    (
        fun(Period, Signals) ->
            OldSignal = element(2, hd(Signals)),
            NewSignal = update_signal(History, Period, OldSignal),
            [{Period, NewSignal}|Signals]
        end,
        [{0, 0}],
        lists:seq(2, MaxPeriod)
    ),
    lists:reverse(lists:filter(fun({Period,_}) -> Period >= MinPeriod end, SignalsRaw)).

update_signal(History, Period, OldSignal) ->
    MA1 = trade_utils:weighted_average(trade_utils:close(lists:sublist(History, Period-1))),
    MA2 = trade_utils:weighted_average(trade_utils:close(lists:sublist(History, Period))),
    OldSignal + MA1 - MA2.

update_ranks(History, Signals, Ranks) ->
    lists:map
    (
        fun({{P, Signal}, {P, Rank}}) -> {P, update_rank(History, Signal, Rank)} end,
        lists:zip(Signals, Ranks)
    ).

update_rank([Bar1,Bar2|_], Signal, Rank) when Signal > 0 ->
    Rank * (trade_utils:close(Bar1) / trade_utils:close(Bar2));

update_rank(_History, _NewSignal, Rank) ->
    Rank.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


run() ->
    Symbol = "LKOH",
    Period = 60,
    trade_tester:test
    (
        [
            {from, {2011, 1, 1}},
            {to,   {2012, 1, 1}},
            {symbol, Symbol},
            {period, Period},
            {money, 100000}
        ],
        pacha,
        [
            {min_period, 30},
            {max_period, 100},
            {learn_period, {{2011, 1, 1}, {2011, 1, 1}}},
            {security, Symbol},
            {period, Period}
        ]
    ).
%
% runtest(N) ->
%     random:seed(erlang:now()),
%     Bids = [ value() || _ <- lists:seq(0, N) ],
%     Results = [ {X/100, calc_geom(X/100, Bids)} || X <- lists:seq(1, 100) ],
%     lists:foreach( fun({X,Y}) -> io:format("~p: ~p~n", [X,Y]) end, lists:sublist(lists:reverse(lists:keysort(2, Results)), 5) ).
%
% value() ->
%     lists:nth(random:uniform(2), [-1,2]).
%
%
% calc_geom(F, Bids) ->
%     MaxLoss = lists:min(Bids),
%     HPRList = lists:map(fun(Bid) -> 1 + F * (-Bid / MaxLoss) end, Bids),
%     trade_utils:geometric_mean(HPRList).


