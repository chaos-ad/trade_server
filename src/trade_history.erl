-module(trade_history).
-compile(export_all).

-define(SERVER, {global, ?MODULE}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    gen_server:start_link(?SERVER, ?MODULE, [], []).

get_history(Symbol, Period, From) ->
    get_history(Symbol, Period, From, trade_utils:local_time()).

get_history(Symbol, Period, From, To) ->
    gen_server:call(?SERVER, {get_history, Symbol, Period, From, To}, infinity).

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

handle_call({get_history, Symbol, Period, From, To}, _, State) ->

    T1 = trade_utils:to_date(From),
    T2 = min( trade_utils:to_date(To), local_yesterday() ),

    case trade_db:get_range(Symbol, Period) of
        undefined ->
            fetch_history(Symbol, Period, T1, T2),
            trade_db:set_range(Symbol, Period, T1, T2),
            {reply, trade_db:get_history(Symbol, Period, From, To), State};
        {Begin, End} ->
            fetch_history(Symbol, Period, T1, Begin),
            fetch_history(Symbol, Period, End, T2),
            trade_db:set_range(Symbol, Period, min(Begin, T1), max(End, T2)),
            {reply, trade_db:get_history(Symbol, Period, From, To), State}
    end.

fetch_history(Symbol, Period, From, To) when From < To ->
    {MarketID, SymbolID} = get_symbol_info(Symbol),
    Data = trade_finam:download_history(MarketID, SymbolID, Period, From, To),
    lager:debug("Importing ~B bars...", [length(Data)]),
    ok = trade_db:add_history(Symbol, Period, Data);

fetch_history(_, _, From, To) when From >= To -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_symbol_info(Symbol) ->
     case trade_db:find_symbol(Symbol) of
        [{ID, _, _, Market}] -> {Market, ID};
        _                    -> exit("symbol not found")
    end.


local_yesterday() ->
    trade_utils:to_date(trade_utils:sub_days(trade_utils:local_time(), 1)).
