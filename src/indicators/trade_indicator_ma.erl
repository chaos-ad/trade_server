-module(trade_indicator_ma).
-compile(export_all).

get_data(History, Args) ->

    Period = trade_arg_utils:get_default(Args, "ma_period", integer, 30),

    Times = trade_utils:time(History),
    Close = trade_utils:close(History),
    Values = trade_utils:moving_average(Period, Close),
    lists:map(fun({T,V}) -> {[{time,T},{value,V}]} end, lists:zip(Times, Values)).


