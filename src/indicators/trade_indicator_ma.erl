-module(trade_indicator_ma).
-compile(export_all).

get_data(History, Options) ->

    {[Period], _} = trade_admin_utils:get_args([{"ma_period", integer, {default, 30}}], Options),

    Times = trade_utils:time(History),
    Close = trade_utils:close(History),
    Values = trade_utils:moving_average(Period, Close),
    lists:map(fun({T,V}) -> {[{time,T},{value,V}]} end, lists:zip(Times, Values)).


