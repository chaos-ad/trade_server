-module(trade_test_strategy).
-compile(export_all).

-include("trade_periods.hrl").

-record(state, {p1, p2, old1=0, old2=0}).

init(P1, P2) -> {ok, #state{p1=P1, p2=P2}}.

update(History, State=#state{p1=P1, p2=P2, old1=0, old2=0}) when length(History) >= P2 ->
    WAP1 = trade_utils:weighted_average(trade_utils:close(lists:sublist(History, P1))),
    WAP2 = trade_utils:weighted_average(trade_utils:close(lists:sublist(History, P2))),
    WAP1_OLD = trade_utils:weighted_average(trade_utils:close(lists:sublist(History, 2, P1))),
    WAP2_OLD = trade_utils:weighted_average(trade_utils:close(lists:sublist(History, 2, P2))),
    case WAP1 > WAP2 andalso WAP1_OLD < WAP2_OLD of
        true  -> {buy, State#state{old1=WAP1, old2=WAP2}};
        false ->
            case WAP1 < WAP2 andalso WAP1_OLD > WAP2_OLD of
                true  -> {sell, State#state{old1=WAP1, old2=WAP2}};
                false -> {ok, State}
            end
    end;

update(History, State=#state{p1=P1, p2=P2, old1=WAP1_OLD, old2=WAP2_OLD}) when length(History) >= P2 ->
    WAP1 = trade_utils:weighted_average(trade_utils:close(lists:sublist(History, P1))),
    WAP2 = trade_utils:weighted_average(trade_utils:close(lists:sublist(History, P2))),
    case WAP1 > WAP2 andalso WAP1_OLD < WAP2_OLD of
        true  -> {buy, State#state{old1=WAP1, old2=WAP2}};
        false ->
            case WAP1 < WAP2 andalso WAP1_OLD > WAP2_OLD of
                true  -> {sell, State#state{old1=WAP1, old2=WAP2}};
                false -> {ok, State}
            end
    end;

update(_, State) -> {ok, State}.
