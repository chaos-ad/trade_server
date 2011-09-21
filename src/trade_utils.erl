-module(trade_utils).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

peername({{A, B, C, D}, Port}) -> %% IPv4 only
    io_lib:format("~p.~p.~p.~p:~p", [A, B, C, D, Port]);

peername({IP, Port}) -> % Will fail on versions, prior to R14B02
    inet_parse:ntoa(IP) ++ ":" ++ integer_to_list(Port);

peername(Socket) ->
    {ok, Peername} = inet:peername(Socket),
    peername(Peername).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TickInfo : {Symbol, Period, Time, Open, High, TickInfoow, Close, Vol}

symbol(TickInfo) when is_list(TickInfo) -> lists:map(fun symbol/1, TickInfo);
symbol(TickInfo) when is_tuple(TickInfo) -> element(1, TickInfo).

period(TickInfo) when is_list(TickInfo) -> lists:map(fun period/1, TickInfo);
period(TickInfo) when is_tuple(TickInfo) -> element(2, TickInfo).

time(TickInfo) when is_list(TickInfo) -> lists:map(fun time/1, TickInfo);
time(TickInfo) when is_tuple(TickInfo) -> element(3, TickInfo).

open(TickInfo) when is_list(TickInfo) -> lists:map(fun open/1, TickInfo);
open(TickInfo) when is_tuple(TickInfo) -> element(4, TickInfo).

high(TickInfo) when is_list(TickInfo) -> lists:map(fun high/1, TickInfo);
high(TickInfo) when is_tuple(TickInfo) -> element(5, TickInfo).

low(TickInfo) when is_list(TickInfo) -> lists:map(fun low/1, TickInfo);
low(TickInfo) when is_tuple(TickInfo) -> element(6, TickInfo).

close(TickInfo) when is_list(TickInfo) -> lists:map(fun close/1, TickInfo);
close(TickInfo) when is_tuple(TickInfo) -> element(7, TickInfo).

volume(TickInfo) when is_list(TickInfo) -> lists:map(fun volume/1, TickInfo);
volume(TickInfo) when is_tuple(TickInfo) -> element(8, TickInfo).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

to_unixtime(Time) when is_integer(Time) ->
    Time - 62167219200;

to_unixtime({{Year, Month, Day}, {Hour, Min, Sec}}) ->
    to_unixtime(calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {Hour, Min, Sec}})).

to_datetime(Timestamp) when is_integer(Timestamp) ->
    calendar:gregorian_seconds_to_datetime(Timestamp + 62167219200).

to_datestr(Timestamp) when is_integer(Timestamp) ->
    to_datestr(to_datetime(Timestamp));

to_datestr({{Year, Month, Day}, {Hour, Min, Sec}}) ->
    lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B", [Year, Month, Day, Hour, Min, Sec])).

local_time() ->
    to_unixtime(calendar:datetime_to_gregorian_seconds(calendar:local_time())).

universal_time() ->
    to_unixtime(calendar:datetime_to_gregorian_seconds(calendar:universal_time())).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

average(Data) when is_list(Data) ->
    lists:sum(Data) / length(Data).

weighted_average(Data) when is_list(Data) ->
    N = length(Data),
    {X, Y} = lists:foldl(fun({X, Y}, {R, S}) -> {R+X*Y, S+X} end, {0, 0}, lists:zip(lists:seq(1, N), Data)),
    X / Y.

moving_average(Period, Data) ->
    Fun = fun(X, {Tmp, Res}) ->
            NewTmp = [X|lists:sublist(Tmp, Period-1)],
            NewAvg = average(NewTmp),
            NewRes = [NewAvg|Res],
            {NewTmp, NewRes}
          end,
    {_, Result} = lists:foldl(Fun, {[], []}, Data),
    lists:reverse(Result).

weighted_moving_average(Period, Data) ->
    Fun = fun(X, {Tmp, Res}) ->
            NewTmp = [X|lists:sublist(Tmp, Period-1)],
            NewAvg = weighted_average(NewTmp),
            NewRes = [NewAvg|Res],
            {NewTmp, NewRes}
          end,
    {_, Result} = lists:foldl(Fun, {[], []}, Data),
    lists:reverse(Result).
