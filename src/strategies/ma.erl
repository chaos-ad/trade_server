-module(ma).
-behavior(simple_strategy).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start_link/1]).
-export([start/1, stop/1, update/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {period1, period2, ma1=0, ma2=0}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Options) ->
    gen_strategy:start_link(?MODULE, Options).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Options) ->
    Period1 = proplists:get_value(period1, Options, 10),
    Period2 = proplists:get_value(period2, Options, 100),
    lager:debug("Strategy ~p started: periods: ~p, ~p", [?MODULE, Period1, Period2]),
    #state{period1=Period1, period2=Period2}.

update(History, State=#state{period1=P1, period2=P2, ma1=0, ma2=0}) when length(History) >= P2 ->
    MA1 = trade_utils:weighted_average(trade_utils:close(lists:sublist(History, P1))),
    MA2 = trade_utils:weighted_average(trade_utils:close(lists:sublist(History, P2))),
    MA1_OLD = trade_utils:weighted_average(trade_utils:close(lists:sublist(History, 2, P1))),
    MA2_OLD = trade_utils:weighted_average(trade_utils:close(lists:sublist(History, 2, P2))),
    case MA1 > MA2 andalso MA1_OLD < MA2_OLD of
        true  -> {buy, State#state{ma1=MA1, ma2=MA2}};
        false ->
            case MA1 < MA2 andalso MA1_OLD > MA2_OLD of
                true  -> {sell, State#state{ma1=MA1, ma2=MA2}};
                false -> {hold, State}
            end
    end;

update(History, State=#state{period1=P1, period2=P2, ma1=MA1_OLD, ma2=MA2_OLD}) when length(History) >= P2 ->
    MA1 = trade_utils:weighted_average(trade_utils:close(lists:sublist(History, P1))),
    MA2 = trade_utils:weighted_average(trade_utils:close(lists:sublist(History, P2))),
    case MA1 > MA2 andalso MA1_OLD < MA2_OLD of
        true  -> {buy, State#state{ma1=MA1, ma2=MA2}};
        false ->
            case MA1 < MA2 andalso MA1_OLD > MA2_OLD of
                true  -> {sell, State#state{ma1=MA1, ma2=MA2}};
                false -> {hold, State}
            end
    end.

stop(_) ->
    lager:debug("Strategy ~p stopped", [?MODULE]),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%