-module(trade_history).
-compile(export_all).

-define(SERVER, {global, ?MODULE}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    gen_server:start_link(?SERVER, ?MODULE, [], []).

download_symbols() ->
    gen_server:call(?SERVER, download_symbols, infinity).

get_history(Symbol, Period, From, To) ->
    gen_server:call(?SERVER, {get_history, Symbol, Period, From, To}, infinity).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) -> {ok, undefined}.

handle_call(download_symbols, _, State) ->
    Symbols = trade_finam:download_symbols(),
    lists:foreach( fun(X) -> trade_db:add_symbol(X) end, Symbols ),
    {reply, {downloaded, length(Symbols)}, State};

handle_call({get_history, Symbol, Period, From, To}, _, State) ->
    case trade_db:get_range(Symbol, Period) of
        undefined ->
            ok = download_history(trade_db, Symbol, Period, From, To),
            {reply, trade_db:get_history(Symbol, Period, From, To), State};
        {Begin, End} ->
            ok = download_history(trade_db, Symbol, Period, From, Begin),
            ok = download_history(trade_db, Symbol, Period, End, To),
            {reply, trade_db:get_history(Symbol, Period, From, To), State}
    end.

download_history(trade_db, Symbol, Period, From, To) ->
    T2 = trade_utils:to_unixtime(To),
    T1 = trade_utils:to_unixtime(From),
    case trade_db:get_range(Symbol, Period) of
        undefined ->
            ok = download_history_simple(trade_db, Symbol, Period, T1, T2),
            ok = trade_db:set_range(Symbol, Period, T1, T2);
        {Begin, End} ->
            case {download_history_simple(trade_db, Symbol, Period, T1, Begin),
                  download_history_simple(trade_db, Symbol, Period, End, T2)} of
                {none, none} -> ok;
                _            -> ok = trade_db:set_range(Symbol, Period, min(T1, Begin), max(T2, End))
            end
    end.

download_history_simple(_, _, _, From, To) when From >= To -> none;
download_history_simple(trade_db, Symbol, Period, From, To) when From <  To ->
    Data = trade_finam:download_history(Symbol, Period, From, To),
    ok = trade_db:add_history(Symbol, Period, Data).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
