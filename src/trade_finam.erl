-module(trade_finam).
-compile(export_all).

-include("trade_timeframes.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% get_history(Symbol, Period, From, To) ->
%     case trade_db:get_history(Symbol, Period, From, To) of
%         [] ->
%     end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

download_history(Symbol, Period, From, To) ->
%     error_logger:info_msg("downloading history...~n"),
    {ok, {{_, 200, _}, _, Body}} = httpc:request(make_url(Symbol, Period, From, To)),
%     error_logger:info_msg("parsing history...~n"),
    parse_downloaded_history(Body).

make_url(Symbol, Period, From, To) ->
    "http://195.128.78.52/get?" ++ make_query(Symbol, Period, From, To).

make_query(Symbol, Period, {YF, MF, DF}, {YT, MT, DT}) ->
    S = get_symbol_code(Symbol),
    P = get_period_code(Period),
    Template = "d=d&market=1&em=~p&df=~p&mf=~p&yf=~p&dt=~p&mt=~p&yt=~p&p=~p&f=filename&e=.csv&cn=~s&dtf=1&tmf=1&MSOR=0&sep=3&sep2=1&datf=1",
    io_lib:format(Template, [S, DF, MF-1, YF, DT, MT-1, YT, P, Symbol]).

get_symbol_code(Symbol) ->
    trade_db:get_finam_symbol_code(Symbol).

get_period_code(?PERIOD_M1)  -> 2;
get_period_code(?PERIOD_M5)  -> 3;
get_period_code(?PERIOD_M15) -> 5;
get_period_code(?PERIOD_M30) -> 6;
get_period_code(?PERIOD_H1)  -> 7;
get_period_code(?PERIOD_D1)  -> 8;
get_period_code(?PERIOD_W1)  -> 9;
get_period_code(?PERIOD_MN1) -> 10.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_downloaded_history(Data) ->
    lists:map(fun parse_downloaded_history_line/1, string:tokens(Data, "\n")).

parse_downloaded_history_line(Line) ->
    [Symbol, Period, Date, Time, Open, High, Low, Close, Volume] = string:tokens(Line, ";\r\n"),
    {Symbol, p(Period), {d(Date), t(Time)}, f(Open), f(High), f(Low), f(Close), i(Volume)}.

d([Y1,Y2,Y3,Y4,M1,M2,D1,D2]) -> {i([Y1,Y2,Y3,Y4]), i([M1,M2]), i([D1,D2])}.
t([H1,H2,M1,M2,S1,S2]) -> {i([H1,H2]), i([M1,M2]), i([S1,S2])}.
f(X) -> list_to_float(X).
i(X) -> list_to_integer(X).

p("D") -> ?PERIOD_D1;
p("W") -> ?PERIOD_W1;
p("M") -> ?PERIOD_MN1;
p(P)   -> i(P).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

