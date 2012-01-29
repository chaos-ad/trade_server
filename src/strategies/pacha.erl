-module(pacha).
-behaviour(bar_strategy).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start/2, stop/2, update/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {min_period, max_period, ranks, old_signals, cur_signals}).

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
        min_period  = MinPeriod,
        max_period  = MaxPeriod,
        ranks       = [ {X,{1,0}} || X <- lists:seq(MinPeriod, MaxPeriod) ],
        old_signals = [ {X,   0 } || X <- lists:seq(MinPeriod, MaxPeriod) ],
        cur_signals = [ {X,   0 } || X <- lists:seq(MinPeriod, MaxPeriod) ]
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

update(History, _Terminal, State=#state{cur_signals=CurSignals, old_signals=OldSignals, ranks=CurRanks}) ->
    NewRanks   = update_ranks(History, OldSignals, CurSignals, CurRanks),
    NewSignals = update_signals(History, State#state.min_period, State#state.min_period),

    {CurPeriod, CurRank}   = hd(lists:reverse(lists:keysort(2, CurRanks))),
    {CurPeriod, CurSignal} = lists:keyfind(CurPeriod, 1, CurSignals),

    {NewPeriod, NewRank}   = hd(lists:reverse(lists:keysort(2, NewRanks))),
    {NewPeriod, NewSignal} = lists:keyfind(NewPeriod, 1, NewSignals),

    Bar  = hd(History),
    Time = trade_utils:to_datetimestr(trade_utils:time(Bar)),
    lager:debug("pacha: ~s: period ~p -> ~p; signal: ~p -> ~p; rank ~p -> ~p", [Time, CurPeriod,NewPeriod,CurSignal,NewSignal,CurRank,NewRank]),

%     lager:debug("pacha: Old rank: ~p~n", [OldRank]),

%     case OldRank =/= NewRank of
%         true  ->
%             Bar  = hd(History),
%             Time = trade_utils:to_datetimestr(trade_utils:time(Bar)),
%             lager:debug("pacha: ~s: Old rank: ~p, New rank: ~p~n", [Time, OldRank, NewRank]);
%         false ->
%             ok
%     end,

    Lots = lots(NewSignal),
    {Lots, State#state{old_signals=CurSignals, cur_signals=NewSignals, ranks=NewRanks}}.

lots(Sig) when Sig  > 0 -> 1;
lots(Sig) when Sig =< 0 -> 0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

stop(_, #state{ranks=_Ranks}) ->
%     lager:debug("pacha: Final ranks: ~p~n", [Ranks]),
%     lager:info("pacha: Ranks:~n"),
%     lists:foreach( fun({X,{Y,Z}}) -> lager:info("pacha: ~p: ~p ~p~n", [X,Y,Z]) end, Ranks ),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_signals(History, MinPeriod, MaxPeriod) ->
    SignalsRaw = lists:foldl
    (
        fun(Period, Signals) ->
            [{Period, update_signal(History, Period, element(2, hd(Signals)))}|Signals]
        end,
        [{0, 0}],
        lists:seq(MinPeriod, MaxPeriod)
    ),
    tl(lists:reverse(SignalsRaw)).

update_signal(History, Period, OldSignal) ->
    MA1 = trade_utils:weighted_average(trade_utils:close(lists:sublist(History, Period-1))),
    MA2 = trade_utils:weighted_average(trade_utils:close(lists:sublist(History, Period))),
    OldSignal + MA1 - MA2.

update_ranks(History, OldSignals, CurSignals, Ranks) ->
    lists:map
    (
        fun({ {{P,OldSignal},{P,CurSignal}}, {P,OldRank} }) ->
            {P, update_rank(History, OldSignal, CurSignal, OldRank)}
        end,
        lists:zip(lists:zip(OldSignals, CurSignals), Ranks)
    ).

update_rank([Bar1,Bar2|_], _OldSignal, CurSignal, {Rank, N}) when CurSignal > 0 ->
    {Rank * (trade_utils:close(Bar1) / trade_utils:close(Bar2)), N+1};

% update_rank([Bar1,Bar2|_], OldSignal, CurSignal, {Rank, N}) when CurSignal =< 0 ->
%     case OldSignal > 0 of
%         true  -> {Rank * (trade_utils:close(Bar1) / trade_utils:close(Bar2)), N+1};
%         false -> {Rank, N}
%     end;

update_rank(_History, _OldSignal, _NewSignal, Rank) ->
    Rank.


run() ->
    Symbol = "LKOH",
    Period = 60,
    trade_tester:test
    (
        [
            {from, {2011, 12, 1}},
            {to,   {2012, 1, 1}},
            {symbol, Symbol},
            {period, Period},
            {money, 100000}
        ],
        pacha,
        [
            {min_period, 80},
            {max_period, 80},
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


