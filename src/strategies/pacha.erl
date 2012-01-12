-module(pacha).
-behaviour(bar_strategy).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start/2, stop/2, update/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {max_period, ranks}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Terminal, Options) ->
    MaxPeriod =
    case proplists:get_value(max_period, Options, 100) of
        Max when Max =< 1 -> exit(invalid_args);
        Max when Max  > 1 -> Max
    end,

    State =
    #state{
        max_period=MaxPeriod,
        ranks=lists:zip(lists:seq(2, MaxPeriod), lists:duplicate(MaxPeriod-1, 1))
    },

    History =
    case proplists:get_value(learn_period, Options) of
        undefined  -> [];
        From ->
            Sec = proplists:get_value(security, Options),
            Period = proplists:get_value(period, Options),
            case From of
                {T1, T2} ->
                    lager:debug("Training using historical data in range [~s : ~s]...", [edate:date_to_string(T1), edate:date_to_string(T2)]),
                    trade_history:get_history(Sec, Period, T1, T2);
                T1 when is_tuple(T1) ->
                    lager:debug("Training using historical data from ~s...", [edate:date_to_string(T1)]),
                    trade_history:get_history(Sec, Period, T1)
            end
    end,
    NewState = learn([], History, Terminal, State),
    lager:debug("Training complete"),
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

update(History, _Terminal, State=#state{max_period=MaxPeriod, ranks=Ranks}) ->
    Signals = signals(History, MaxPeriod),
    NewRanks = recalc_ranks(History, Signals, Ranks),
%     io:format("New signals: ~p~n", [Signals]),
%     io:format("New ranks: ~p~n", [NewRanks]),
    {BestPeriod, _} = hd(lists:reverse(lists:keysort(2, NewRanks))),
%     io:format("Best period: ~p~n", [BestPeriod]),
    {_, BestSignal} = lists:keyfind(BestPeriod, 1, Signals),
%     io:format("Signal: ~p~n", [BestSignal]),
    {lots(BestSignal), State#state{ranks=NewRanks}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

stop(_, _) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lots(Signal) when Signal  > 0 -> 1;
lots(Signal) when Signal =< 0 -> 0.

signals(History, MaxPeriod) ->
    SignalsRaw = lists:foldl(
        fun(Period, Signals) ->
            [{Period, signal(History, Period, element(2, hd(Signals)))}|Signals]
        end,
        [{0, 0}],
        lists:seq(2, MaxPeriod)
    ),
    tl(lists:reverse(SignalsRaw)).

signal(History, Period, OldSignal) ->
    MA1 = trade_utils:weighted_average(trade_utils:close(lists:sublist(History, Period-1))),
    MA2 = trade_utils:weighted_average(trade_utils:close(lists:sublist(History, Period))),
    OldSignal + MA1 - MA2.



recalc_ranks(History, Signals, Ranks) ->
    lists:map
    (
        fun({{N, Signal}, {N, Rank}}) -> {N, update_rank(History, Signal, Rank)} end,
        lists:zip(Signals, Ranks)
    ).

update_rank([Bar1,Bar2|_], Signal, Rank) when Signal > 0 ->
    Rank * (1 + (trade_utils:close(Bar1) - trade_utils:close(Bar2)) / trade_utils:close(Bar2));

update_rank([Bar1,Bar2|_], Signal, Rank) when Signal =< 0 ->
    Rank * (1 - (trade_utils:close(Bar1) - trade_utils:close(Bar2)) / trade_utils:close(Bar2)).
