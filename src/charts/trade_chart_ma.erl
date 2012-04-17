-module(trade_chart_ma).
-compile(export_all).

get_data(History, Args) ->

    Times = trade_utils:time(History),
    Close = trade_utils:close(History),
    Security = trade_arg_utils:get_required(Args, "security"),

    DataSeries =
    [
        {[
            {data, lists:map(fun({T,V}) -> [T*1000,V] end, lists:zip(Times, trade_utils:moving_average(30, Close)))},
            {name, <<"Moving average 30">>},
            {tooltip, {[{yDecimals, 2}]}}
        ]},
        {[
            {data, lists:map(fun({T,V}) -> [T*1000,V] end, lists:zip(Times, trade_utils:moving_average(100, Close)))},
            {name, <<"Moving average 100">>},
            {tooltip, {[{yDecimals, 2}]}}
        ]}
    ],

    YAxis = [{[
        {title, {[
            {text, <<"Data">>}
        ]}},
        {height, 200},
        {lineWidth, 2}
    ]}, {[
        {title, {[
            {text, <<"Volume">>}
        ]}},
        {top, 300},
        {height, 100},
        {offset, 0},
        {lineWidth, 2}
    ]}],

    HistorySeries =
    {[
        {data, lists:map(fun({T,O,H,L,C,_}) -> [T*1000,O,H,L,C] end, History)},
        {name, list_to_binary(Security)},
        {type, <<"candlestick">>},
        {shadow, true},
        {tooltip, {[{yDecimals, 2}]}}
    ]},
    VolumeSeries =
    {[
        {data, lists:map(fun({T,_,_,_,_,V}) -> [T*1000,V] end, History)},
        {type, <<"column">>},
        {name, <<"Volume">>},
        {yAxis, 1}
    ]},
    Series = [HistorySeries, VolumeSeries] ++ DataSeries,

    {[
        {yAxis, YAxis},
        {series, Series}
    ]}.

