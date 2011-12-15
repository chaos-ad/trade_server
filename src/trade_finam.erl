-module(trade_finam).
-compile(export_all).

-include("trade_periods.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

download_symbols() ->
    {ok, {{_, 200, _}, _, Body}} = httpc:request("http://www.finam.ru/scripts/export.js"),
    {match, [[L1], [L2], [L3], [L4], _]} = re:run(Body, ".*Array\\((.*)\\);", [dotall, ungreedy, global, {capture, [1], list}]),
    Ids     = lists:map(fun(X) -> list_to_integer(X) end, string:tokens(L1, ",")),
    Names   = lists:map(fun prepare_symbols/1, string:tokens(L2, ",")),
    Codes   = lists:map(fun prepare_symbols/1, string:tokens(L3, ",")),
    Markets = lists:map(fun(X) -> list_to_integer(X) end, string:tokens(L4, ",")),
    ZipFn   = fun({ID, Code}, {Name, Market}) -> {ID, Code, Name, Market} end,
    lists:zipwith(ZipFn, lists:zip(Ids, Codes), lists:zip(Names, Markets)).

prepare_symbols(Str) ->
    {ok, R} = iconverl:conv("utf-8", "cp1251", list_to_binary(string:strip(Str, both, $'))), R.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_symbol_info(Symbol) ->
     case trade_db:find_symbol(Symbol) of
        [{ID, _, _, Market}] -> {Market, ID};
        _                    -> exit("symbol not found")
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

download_history(Symbol, ?PERIOD_M1, From, To) when From < To ->
    T1 = trade_utils:to_date(From),
    T2 = trade_utils:to_date(To),
    {MarketID, SymbolID} = get_symbol_info(Symbol),
    download_by_year(MarketID, {Symbol, SymbolID}, ?PERIOD_M1, T1, T2, []);

download_history(Symbol, Period, From, To) when From < To ->
    T1 = trade_utils:to_date(From),
    T2 = trade_utils:to_date(To),
    {MarketID, SymbolID} = get_symbol_info(Symbol),
    download_by_year(MarketID, {Symbol, SymbolID}, Period, T1, T2, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PERIOD_M1 can be downloaded only by 1 year

download_by_year(_, _, _, T1, T1, Acc) ->
    lists:concat(lists:reverse(Acc));

download_by_year(MarketID, SymbolID, Period, T1={Year,_,_}, T2={Year,_,_}, Acc) ->
    Part = download(MarketID, SymbolID, Period, T1, T2),
    download_by_year(MarketID, SymbolID, Period, T2, T2, [Part|Acc]);

download_by_year(MarketID, SymbolID, Period, T1={Year,_,_}, T2, Acc) ->
    Part = download(MarketID, SymbolID, Period, T1, {Year, 12, 31}),
    download_by_year(MarketID, SymbolID, Period, {Year+1, 1, 1}, T2, [Part|Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

download(MarketID, {Symbol, SymbolID}, Period, T1, T2) when T1 < T2 ->
    lager:debug("Downloading range ~s - ~s...", [trade_utils:to_datestr(T1), trade_utils:to_datestr(T2)]),
    URL = make_url(MarketID, SymbolID, Period, T1, T2),
    lager:debug("Downloading '~s'...", [URL]),
    {ok, {{_, 200, _}, _, Body}} = httpc:request(URL),
    lager:debug("Parsing..."),
    History = parse_history(Body),
    lager:debug("Importing ~B bars...", [length(History)]),
    ok = trade_db:add_history(Symbol, Period, History),
    History.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_url(MarketID, SymbolID, Period, From, To) ->
    "http://195.128.78.52/get?" ++ make_query(MarketID, SymbolID, Period, From, To).

make_query(MarketID, SymbolID, Period, {YearFrom, MonthFrom, DayFrom}, {YearTo, MonthTo, DayTo}) ->
    PeriodCode = get_period_code(Period),
    Template = "d=d&market=~B&em=~B&df=~p&mf=~p&yf=~p&dt=~p&mt=~p&yt=~p&p=~p&f=filename&e=.csv&cn=SHIT&dtf=1&tmf=1&MSOR=0&sep=3&sep2=1&datf=5",
    io_lib:format(Template, [MarketID, SymbolID, DayFrom, MonthFrom-1, YearFrom, DayTo, MonthTo-1, YearTo, PeriodCode]).

get_period_code(?PERIOD_M1)  -> 2;
get_period_code(?PERIOD_M5)  -> 3;
get_period_code(?PERIOD_M15) -> 5;
get_period_code(?PERIOD_M30) -> 6;
get_period_code(?PERIOD_H1)  -> 7;
get_period_code(?PERIOD_D1)  -> 8;
get_period_code(?PERIOD_W1)  -> 9;
get_period_code(?PERIOD_MN1) -> 10.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_history(Data) ->
    lists:map(fun parse_history_line/1, string:tokens(Data, "\n")).

parse_history_line(Line) ->
    [Date, Time, Open, High, Low, Close, Volume] = string:tokens(Line, ";\r\n"),
    {{d(Date), t(Time)}, f(Open), f(High), f(Low), f(Close), i(Volume)}.

d([Y1,Y2,Y3,Y4,M1,M2,D1,D2]) -> {i([Y1,Y2,Y3,Y4]), i([M1,M2]), i([D1,D2])}.
t([H1,H2,M1,M2,S1,S2]) -> {i([H1,H2]), i([M1,M2]), i([S1,S2])}.
f(X) -> list_to_float(X).
i(X) -> list_to_integer(X).

p("D") -> ?PERIOD_D1;
p("W") -> ?PERIOD_W1;
p("M") -> ?PERIOD_MN1;
p(P)   -> i(P).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

