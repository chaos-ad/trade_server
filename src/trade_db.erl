-module(trade_db).
-behavior(gen_server).
-compile(export_all).

-include("trade_periods.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-define(SERVER, {global, ?MODULE}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(symbol, {id, code, name, market}).
-record(bar, {timestamp, open, high, low, close, volume}).
-record(range, {tab, from, to}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    gen_server:start_link(?SERVER, ?MODULE, [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_symbol({ID, Code, Name, Market}) ->
    gen_server:call(?SERVER, {add_symbol, {ID, Code, Name, Market}}).

get_symbol(Arg) ->
    gen_server:call(?SERVER, {get_symbol, Arg}).

find_symbol(Arg) ->
    gen_server:call(?SERVER, {find_symbol, Arg}).

get_symbols_count() ->
    gen_server:call(?SERVER, get_symbols_count).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_history(Symbol, Period, Ticks) ->
    gen_server:call(?SERVER, {add_history, Symbol, Period, Ticks}, infinity).

get_history(Symbol, Period, Timestamp) ->
    gen_server:call(?SERVER, {get_history, Symbol, Period, Timestamp}, infinity).

get_history(Symbol, Period, From, To) ->
    T1 = trade_utils:to_unixtime(From),
    T2 = trade_utils:to_unixtime(To),
    gen_server:call(?SERVER, {get_history, Symbol, Period, T1, T2}, infinity).

del_history(Symbol, Period) ->
    gen_server:call(?SERVER, {del_history, Symbol, Period}, infinity).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_range(Symbol, Period) ->
    gen_server:call(?SERVER, {get_range, Symbol, Period}, infinity).

set_range(Symbol, Period, {T1, T2}) ->
    gen_server:call(?SERVER, {set_range, Symbol, Period, {T1, T2}}, infinity).

del_range(Symbol, Period) ->
    gen_server:call(?SERVER, {del_range, Symbol, Period}, infinity).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
    process_flag(trap_exit, true),
    case mnesia:create_schema([node()]) of
        ok ->
            ok = mnesia:start(),
            lager:info("Creating database..."),
            {atomic, ok} = mnesia:create_table(symbol,[
                {disc_copies, [node()]},
                {attributes, record_info(fields, symbol)},
                {index, [code]}
            ]),
            {atomic, ok} = mnesia:create_table(range,[
                {disc_copies, [node()]},
                {attributes, record_info(fields, range)}
            ]),
            lager:info("Database created");
        {error,{_,{already_exists,_}}} ->
            lager:info("Starting database..."),
            ok = mnesia:start(),
            lager:info("Database started")
    end,
    ok = mnesia:wait_for_tables([symbol, range], 30000),
    {ok, undefined}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({add_symbol, SymbolInfo}, _, State) ->
    {reply, add_symbol_info(SymbolInfo), State};

handle_call({get_symbol, Arg}, _, State) ->
   {reply, get_symbol_info(Arg), State};

handle_call({find_symbol, Arg}, _, State) ->
   {reply, find_symbol_info(Arg), State};

handle_call(get_symbols_count, _, State) ->
    {reply, get_symbol_info_count(), State};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({add_history, Symbol, Period, Bars}, _, State) ->
    {reply, put_bars(Symbol, Period, Bars), State};

handle_call({get_history, Symbol, Period, T1, T2}, _, State) ->
    {reply, get_bars(Symbol, Period, T1, T2), State};

handle_call({del_history, Symbol, Period}, _, State) ->
    {reply, del_bars(Symbol, Period), State};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({get_range, Symbol, Period}, _, State) ->
    {reply, get_bar_range(Symbol, Period), State};

handle_call({del_range, Symbol, Period}, _, State) ->
    {reply, del_bar_range(Symbol, Period), State};

handle_call({set_range, Symbol, Period, {T1, T2}}, _, State) ->
    {reply, set_bar_range(Symbol, Period, {T1, T2}), State};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call(_, _, State) -> {noreply, State}.
handle_cast(_, State) -> {noreply, State}.
handle_info(_, State) -> {noreply, State}.
code_change(_, State, _) -> {ok, State}.

terminate(Reason, _State) ->
    lager:info("Stopping trade database with reason ~p", [Reason]),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_symbol_info({ID, Code, Name, Market}) ->
    mnesia:dirty_write(#symbol{id=ID, code=Code, name=Name, market=Market}).

get_symbol_info(Arg) ->
    case find_symbol_info(Arg) of
        [Info] -> Info;
        _      -> exit("symbol not found")
    end.

find_symbol_info(ID) when is_integer(ID) ->
    case mnesia:dirty_read({symbol, ID}) of
        {symbol, ID, C, N, M} -> {ID, C, N, M};
        _                     -> undefined
    end;

find_symbol_info(Code) when is_list(Code) ->
    find_symbol_info(list_to_binary(Code));

find_symbol_info(Code) when is_binary(Code) ->
    Fn = fun({symbol, ID, C, N, M}) -> {ID, C, N, M} end,
    lists:map(Fn, mnesia:dirty_match_object(#symbol{id='_', code=Code, name='_', market='_'}) ).

get_symbol_info_count() ->
    mnesia:table_info(symbol, size).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

put_bars(Symbol, Period, Bars) when is_list(Bars) ->
    Tab = get_tablename(Symbol, Period),
    ok = create_table(Tab),
    PutFn = fun({Time, Open, High, Low, Close, Volume}) ->
                Bar = {Tab, trade_utils:to_unixtime(Time), Open, High, Low, Close, Volume},
                mnesia:dirty_write(Tab, Bar)
            end,
    lists:foreach(PutFn, Bars).

get_bars(Symbol, Period, T1, T2) ->
    Tab = get_tablename(Symbol, Period),
    case table_exists(Tab) of
        true ->
            ok = mnesia:wait_for_tables([Tab], 30000),
            MatchHead = {Tab, '$1', '_', '_', '_', '_', '_'},
            Guard = {'andalso', {'>=', '$1', T1}, {'=<', '$1', T2}},
            Bars = mnesia:dirty_select(Tab,[{MatchHead, [Guard], ['$_']}]),
            [ {T, O, H, L, C, V} || {_, T, O, H, L, C, V} <- Bars ];
        false ->
            []
    end.

del_bars(Symbol, Period) ->
    ok = del_bar_range(Symbol, Period),
    ok = drop_table(get_tablename(Symbol, Period)).

get_bar_range(Symbol, Period) ->
    Tab = get_tablename(Symbol, Period),
    case mnesia:dirty_read({range, Tab}) of
        [{range, Tab, T1, T2}] -> {T1, T2};
        _                        -> undefined
    end.

del_bar_range(Symbol, Period) ->
    mnesia:dirty_delete({range, get_tablename(Symbol, Period)}).

set_bar_range(Symbol, Period, {T1, T2}) ->
    mnesia:dirty_write({range, get_tablename(Symbol, Period), T1, T2}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local functions:

get_tablename(Symbol, Period) when is_integer(Period) ->
    {ID, _, _, _} = get_symbol_info(Symbol),
    list_to_atom(lists:flatten(io_lib:format("history_~B_~B", [ID, Period]))).

create_table(Name) when is_atom(Name) ->
    case mnesia:create_table(Name, [{disc_copies, [node()]}, {attributes, record_info(fields, bar)}, {type, ordered_set}]) of
        {atomic, ok}                  -> ok;
        {aborted,{already_exists, _}} -> ok
    end.

drop_table(Name) ->
    case mnesia:delete_table(Name) of
        {atomic, ok} -> ok;
        {aborted,{no_exists, _}} -> ok
    end.

table_exists(Table) ->
    try mnesia:table_info(Table, type) of
        _ -> true
    catch
        exit:{aborted, {no_exists, _, _}} -> false
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%