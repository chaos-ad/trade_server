-module(trade_utils).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TickInfo : {Symbol, Period, Time, Open, High, TickInfoow, Close, Vol}

time(TickInfo) when is_list(TickInfo) -> lists:map(fun time/1, TickInfo);
time(TickInfo) when is_tuple(TickInfo) -> element(1, TickInfo).

open(TickInfo) when is_list(TickInfo) -> lists:map(fun open/1, TickInfo);
open(TickInfo) when is_tuple(TickInfo) -> element(2, TickInfo).

high(TickInfo) when is_list(TickInfo) -> lists:map(fun high/1, TickInfo);
high(TickInfo) when is_tuple(TickInfo) -> element(3, TickInfo).

low(TickInfo) when is_list(TickInfo) -> lists:map(fun low/1, TickInfo);
low(TickInfo) when is_tuple(TickInfo) -> element(4, TickInfo).

close(TickInfo) when is_list(TickInfo) -> lists:map(fun close/1, TickInfo);
close(TickInfo) when is_tuple(TickInfo) -> element(5, TickInfo).

volume(TickInfo) when is_list(TickInfo) -> lists:map(fun volume/1, TickInfo);
volume(TickInfo) when is_tuple(TickInfo) -> element(6, TickInfo).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

to_unixtime(Time) when is_integer(Time) -> Time;

to_unixtime({Year, Month, Day}) ->
    to_unixtime({{Year, Month, Day}, {0, 0, 0}});

to_unixtime({{Year, Month, Day}, {Hour, Min, Sec}}) ->
    calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {Hour, Min, Sec}}) - 62167219200.


to_datetime(Timestamp) when is_integer(Timestamp) ->
    calendar:gregorian_seconds_to_datetime(Timestamp + 62167219200);

to_datetime({Year, Month, Day}) ->
    {{Year, Month, Day}, {0, 0, 0}};

to_datetime({{Year, Month, Day}, {Hour, Min, Sec}}) ->
    {{Year, Month, Day}, {Hour, Min, Sec}}.


to_datetimestr({{Year, Month, Day}, {Hour, Min, Sec}}) ->
    lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B", [Year, Month, Day, Hour, Min, Sec]));

to_datetimestr(Value) ->
    to_datetimestr(to_datetime(Value)).


to_date(Value) ->
    element(1, to_datetime(Value)).


to_datestr({Year, Month, Day}) ->
    lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B", [Year, Month, Day]));

to_datestr(Value) ->
    to_datestr(to_date(Value)).


local_time() ->
    to_unixtime(calendar:local_time()).

universal_time() ->
    to_unixtime(calendar:universal_time()).

add_days(Point, Days) -> add_duration(Point, {Days, 0, 0, 0}).
add_hours(Point, Hours) -> add_duration(Point, {Hours, 0, 0}).
add_minutes(Point, Minutes) -> add_duration(Point, {0, Minutes, 0}).
add_seconds(Point, Seconds) -> add_duration(Point, {0, 0, Seconds}).

add_duration(Point, Duration) when is_integer(Point) -> Point + duration_to_seconds(Duration).

sub_days(Point, Days) -> sub_duration(Point, {Days, 0, 0, 0}).
sub_hours(Point, Hours) -> sub_duration(Point, {Hours, 0, 0}).
sub_minutes(Point, Minutes) -> sub_duration(Point, {0, Minutes, 0}).
sub_seconds(Point, Seconds) -> sub_duration(Point, {0, 0, Seconds}).
sub_duration(Point, Duration) -> Point - duration_to_seconds(Duration).

duration_to_seconds({H, M, S}) -> (H*60+M)*60+S;
duration_to_seconds({D, H, M, S}) -> duration_to_seconds({D*24+H, M, S}).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
