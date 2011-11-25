-module(trade_history).
-compile(export_all).

-include("trade_periods.hrl").

-define(SERVER, {global, ?MODULE}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    gen_server:start_link(?SERVER, ?MODULE, [], []).

get_history(Symbol, Period, From) ->
    get_history(Symbol, Period, From, last_closed_bar(Period)).

get_history(Symbol, Period, From, To) ->
    {T1, T2} = get_range(Period, From, To),
    gen_server:call(?SERVER, {get_history, Symbol, Period, T1, T2}, infinity).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
    case trade_db:get_symbols_count() =:= 0 of
        false -> ok;
        true  ->
            lager:info("Initializing symbols..."),
            Symbols = trade_finam:download_symbols(),
            lager:info("Importing ~B symbols...", [length(Symbols)]),
            lists:foreach( fun(X) -> trade_db:add_symbol(X) end, Symbols ),
            lager:info("Symbols initialized")
    end,
    {ok, undefined}.

handle_call({get_history, Symbol, Period, T1, T2}, _, State) ->
    case trade_db:get_range(Symbol, Period) of
        undefined ->
            fetch_history(Symbol, Period, T1, T2),
            trade_db:set_range(Symbol, Period, {T1, T2}),
            {reply, trade_db:get_history(Symbol, Period, T1, T2), State};
        {HistT1, HistT2} ->
            fetch_history(Symbol, Period, T1, HistT1),
            fetch_history(Symbol, Period, HistT2, T2),
            trade_db:set_range(Symbol, Period, {min(HistT1, T1), max(HistT2, T2)}),
            {reply, trade_db:get_history(Symbol, Period, T1, T2), State}
    end.

fetch_history(Symbol, Period, T1, T2) when T1 < T2 ->
    {MarketID, SymbolID} = get_symbol_info(Symbol),
    Data = trade_finam:download_history(MarketID, SymbolID, Period, T1, T2),
    lager:debug("Importing ~B bars...", [length(Data)]),
    ok = trade_db:add_history(Symbol, Period, Data);

fetch_history(_, _, T1, T2) when T1 >= T2 -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_symbol_info(Symbol) ->
     case trade_db:find_symbol(Symbol) of
        [{ID, _, _, Market}] -> {Market, ID};
        _                    -> exit("symbol not found")
    end.

get_range(Period, T1, T2) ->
    D1 = trade_utils:to_date(T1),
    D2 = trade_utils:to_date(T2),
    LastBar = last_closed_bar(Period),
    case D1 > D2 orelse D1 > LastBar orelse D2 > LastBar of
        true  -> exit(invalid_range);
        false -> {D1, D2}
    end.

last_closed_bar(?PERIOD_W1)  -> edate:shift(-calendar:day_of_the_week(edate:today()), day);
last_closed_bar(?PERIOD_MN1) -> edate:end_of_month(edate:shift(-1, month));
last_closed_bar(_)           -> edate:yesterday().

