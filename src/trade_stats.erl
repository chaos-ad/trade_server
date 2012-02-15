-module(trade_stats).
-compile(export_all).

-record(stats, {
    bids=[],
    profits=[],
    positions=[],
    bar_last,
    bar_first,
    money_last,
    money_first
}).

new() -> #stats{}.

set_money(Money, Stats=#stats{money_first=undefined, money_last=undefined}) when Money =/= undefined ->
    Stats#stats{money_first=Money, money_last=Money};

set_money(Money, Stats=#stats{}) when Money =/= undefined ->
    Stats#stats{money_last=Money}.

add_bar(Bar, Stats=#stats{bar_first=undefined, bar_last=undefined}) when Bar =/= undefined ->
    Stats#stats{bar_first=Bar, bar_last=Bar};

add_bar(Bar, Stats=#stats{}) when Bar =/= undefined ->
    Stats#stats{bar_last=Bar}.

buy(Bar, Security, Lots, Price, Stats=#stats{bids=Bids, positions=Positions}) ->
    log_bid({buy, {trade_utils:time(Bar), Lots, Price}}),
    NewBids = [{buy, {trade_utils:time(Bar), Lots, Price}}|Bids],
    case lists:keyfind(Security, 1, Positions) of
        false ->
            NewPositions = [{Security, Lots, Price}|Positions],
            Stats#stats{positions=NewPositions, bids=NewBids};
        {Security, OldLots, AvgPrice} ->
            NewLots = OldLots+Lots,
            NewAvgPrice = ((AvgPrice*OldLots) + (Price*Lots)) / NewLots,
            NewPositions = lists:keyreplace(Security, 1, Positions, {Security, NewLots, NewAvgPrice}),
            Stats#stats{bids=NewBids, positions=NewPositions}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sell(Bar, Security, Lots, Price, Stats=#stats{bids=Bids, positions=Positions, profits=Profits}) ->
    case lists:keyfind(Security, 1, Positions) of
        {Security, OldLots, AvgPrice} when OldLots >= Lots ->
            log_bid({sell, {trade_utils:time(Bar), Lots, AvgPrice, Price}}),
            RelProfit = (Lots*Price)/(Lots*AvgPrice),
            AbsProfit = (Lots*Price)-(Lots*AvgPrice),
            NewPositions =
            case OldLots-Lots of
                0       -> lists:keydelete(Security, 1, Positions);
                NewLots -> lists:keyreplace(Security, 1, Positions, {Security, NewLots, AvgPrice})
            end,
            NewBids = [{sell, {trade_utils:time(Bar), Lots, AvgPrice, Price}}|Bids],
            Stats#stats{bids=NewBids, positions=NewPositions, profits=[{RelProfit, AbsProfit}|Profits]};
        _ ->
            exit("short positions is not implemented")
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_report(Stats=#stats{bids=Bids, profits=Profits, positions=Positions}) ->
    Stats#stats{
        bids=lists:reverse(Bids),
        profits=lists:reverse(Profits),
        positions=lists:reverse(Positions)
    }.

print_report(Stats=#stats{profits=Profits}) ->
    {RelPL, _AbsPL} = lists:unzip(Profits),
    Bids = length(RelPL),
    Wons = length(lists:filter(fun(X) -> X > 1 end, RelPL)),
%     BarLast = Stats#stats.bar_last,
%     BarFirst = Stats#stats.bar_first,
    MoneyFinal = Stats#stats.money_last,
    MoneyFirst = Stats#stats.money_first,
%     PriceLast = trade_utils:close(BarLast),
%     PriceFirst = trade_utils:open(BarFirst),
    lager:info("====================================="),
    lager:info("Win bids:                       ~p~n", [Wons]),
    lager:info("Total bids:                     ~p~n", [Bids]),
    lager:info("Percent of wins:                ~p~n", [percent(Wons, Bids)]),
    lager:info("Start equity:                   ~p~n", [MoneyFirst]),
    lager:info("Final equity:                   ~p~n", [MoneyFinal]),
    lager:info("Total profit:                   ~p~n", [MoneyFinal-MoneyFirst]),

%     lager:info("Geometric mean:                 ~p~n", [trade_utils:geometric_mean(RelPL)]),
%     lager:info("Arithmetic mean (rub):          ~p~n", [trade_utils:arithmetic_mean(AbsPL)]),
%     lager:info("Arithmetic mean normalized:     ~p~n", [trade_utils:arithmetic_mean(RelPL)]),
%     lager:info("Standard derivation (rub):      ~p~n", [trade_utils:standard_derivation(AbsPL)]),
%     lager:info("Standard derivation normalized: ~p~n", [trade_utils:standard_derivation(RelPL)]),
%     lager:info("Real profit (rub):              ~.2f~n", [(MoneyLast-MoneyFirst)]),
%     lager:info("Real profit (%):                ~.2f%~n", [(MoneyLast/MoneyFirst-1)*100]),
%     lager:info("Buy&Hold profit (%):            ~.2f%~n", [(PriceFirst/PriceLast-1)*100]),
%     lager:info("Geometric profit:               ~.2f%~n", [(trade_utils:multiply(RelPL)-1)*100]),
    lager:info("=====================================")
    .

print_bids(#stats{bids=Bids}) ->
    lists:foreach(fun log_bid/1, Bids).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

log_bid({buy, {_Time, _Lots, _Price}}) ->
    Args = [trade_utils:to_datetimestr(_Time), _Lots, (_Lots*_Price)],
    lager:info("~s |  buy | lots: ~5.b | price: ~10.4f rub |", Args);

log_bid({sell, {_Time, _Lots, _AvgPrice, _Price}}) ->
    RelProfit = (_Lots*_Price)/(_Lots*_AvgPrice),
    AbsProfit = (_Lots*_Price)-(_Lots*_AvgPrice),
    Args = [trade_utils:to_datetimestr(_Time), _Lots, (_Lots*_Price), AbsProfit, (RelProfit-1)*100],
    lager:info("~s | sell | lots: ~5.b | price: ~10.4f rub | profit: ~10.4f rub | percent: ~8.4f% |", Args).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

percent(_, 0) -> 0.0;
percent(X, Y) -> X/Y*100.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

save_stats(File, #stats{bids=Bids}) ->
    {ok, FD} = file:open(File, [write]),
    lists:foreach
    (
        fun({buy, {Time,Lots,  Price}}) -> io:format(FD, "~s;~p;~p~n", [trade_utils:to_datetimestr(Time),  Lots, Price]);
           ({sell,{Time,Lots,_,Price}}) -> io:format(FD, "~s;~p;~p~n", [trade_utils:to_datetimestr(Time), -Lots, Price])
        end,
        Bids
    ).
