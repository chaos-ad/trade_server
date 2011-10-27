-module(trade_db_mysql).
-behavior(gen_server).
-compile(export_all).

-include("trade_periods.hrl").
-include_lib("emysql/include/emysql.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-define(DUPLICATE_ENTRY,        1062).
-define(UNKNOWN_TABLE,          1051).
-define(TABLE_ALREADY_EXISTS,   1050).
-define(TABLE_DOES_NOT_EXISTS,  1146).

-define(SERVER, {global, ?MODULE}).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

import_history(Symbol, Period, Ticks) ->
    gen_server:call(?SERVER, {import_history, Symbol, Period, Ticks}, 60000).

get_history(Symbol, Period, Timestamp) ->
    gen_server:call(?SERVER, {get_history, Symbol, Period, Timestamp}, 60000).

get_history(Symbol, Period, From, To) ->
    gen_server:call(?SERVER, {get_history, Symbol, Period, From, To}, 60000).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
    process_flag(trap_exit, true),

    {ok, Options} = application:get_env(database),
    Port = proplists:get_value(port, Options, 3306),
    Host = proplists:get_value(host, Options, "localhost"),
    User = proplists:get_value(user, Options, "trade"),
    Pass = proplists:get_value(pass, Options, "trade"),
    Name = proplists:get_value(name, Options, "trade"),

    lager:info("connecting to database ~s:~B...", [Host, Port]),
    ok = emysql:add_pool(?MODULE, 1, User, Pass, Host, Port, Name, utf8),
    ok = init_database(),
    {ok, undefined}.

init_database() ->
    QueryBin =
    <<"CREATE TABLE `symbols` ("
        "`ID`     INT(10)      UNSIGNED NOT NULL,"
        "`Code`   VARCHAR(10)           NOT NULL,"
        "`Name`   VARCHAR(30)           NOT NULL,"
        "`Market` INT(10)      UNSIGNED NOT NULL,"
        "PRIMARY KEY (`ID`,`Code`)"
        ") ENGINE=MyISAM DEFAULT CHARSET=utf8;">>,
    case execute(QueryBin) of
        R when is_record(R, ok_packet) -> ok;
        R when is_record(R, error_packet) ->
            case R#error_packet.code =:= ?TABLE_ALREADY_EXISTS of
                true -> ok;
                false -> throw_error(R)
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({add_symbol, SymbolInfo}, _, State) ->
    {reply, add_symbol_info(SymbolInfo), State};

handle_call({get_symbol, Arg}, _, State) ->
   {reply, get_symbol_info(Arg), State};

handle_call({find_symbol, Arg}, _, State) ->
   {reply, find_symbol_info(Arg), State};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({import_history, Symbol, Period, Bars}, _, State) ->
    {reply, put_bars(Symbol, Period, Bars), State};

handle_call({get_history, Symbol, Period, From, To}, _, State) ->
    {reply, get_bars(Symbol, Period, From, To), State};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call(_, _, State) -> {noreply, State}.
handle_cast(_, State) -> {noreply, State}.
handle_info(_, State) -> {noreply, State}.
code_change(_, State, _) -> {ok, State}.

terminate(Reason, _State) ->
    emysql:remove_pool(?MODULE),
    lager:info("terminated with reason ~p", [Reason]),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_symbol_info({ID, Code, Name, Market}) ->
    Query = "INSERT INTO symbols VALUES (~B, ~s, ~s, ~B)",
    QueryBin = iolist_to_binary(io_lib:format(Query, [ID, emysql_util:quote(Code), emysql_util:quote(Name), Market])),
    case execute(QueryBin) of
        R when is_record(R, ok_packet)    -> ok;
        R when is_record(R, error_packet) -> throw_error(R)
    end.

get_symbol_info(Arg) ->
    case find_symbol_info(Arg) of
        [Info] -> Info;
        _      -> exit("symbol not found")
    end.

find_symbol_info(ID) when is_integer(ID) ->
    Query = "SELECT * FROM symbols WHERE ID = ~B",
    QueryBin = iolist_to_binary(io_lib:format(Query, [ID])),
    case execute(QueryBin) of
        R when is_record(R, error_packet)  -> throw_error(R);
        R when is_record(R, result_packet) -> lists:map(fun list_to_tuple/1, R#result_packet.rows)
    end;

find_symbol_info(Code) when is_list(Code) or is_binary(Code) ->
    Query = "SELECT * FROM symbols WHERE Code LIKE ~s",
    QueryBin = iolist_to_binary(io_lib:format(Query, [emysql_util:quote(Code)])),
    case execute(QueryBin) of
        R when is_record(R, error_packet)  -> throw_error(R);
        R when is_record(R, result_packet) -> lists:map(fun list_to_tuple/1, R#result_packet.rows)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

put_bars(_, _, []) -> ok;
put_bars(Symbol, Period, Bars) when is_list(Bars) ->

    TableName = get_tablename(Symbol, Period),
    ok = create_table(TableName),

    SubQuery = build_import_query(Bars),
    QueryBin = <<"INSERT INTO ", TableName/binary, " VALUES ", SubQuery/binary>>,

    case execute(QueryBin) of
        R when is_record(R, ok_packet)    -> ok;
        R when is_record(R, error_packet) -> throw_error(R)
    end.

get_bars(Symbol, Period, From, To) when is_tuple(From), is_tuple(To) ->

    TableName = get_tablename(Symbol, Period),

    Query = "SELECT * FROM ~s WHERE Timestamp >= ~B AND Timestamp <= ~B",
    QueryBin = iolist_to_binary(io_lib:format(Query, [TableName, to_unixtime(From), to_unixtime(To)])),

    lager:info("Starting query..."),
    {T1, QRes} = timer:tc( fun() -> execute(QueryBin) end ),
    case QRes of
        R when is_record(R, result_packet) ->
            lager:info("Query takes ~B...", [T1]),
            lager:info("Parsing bars..."),
            {T2, Result} = timer:tc(fun() -> lists:map(fun parse_bar/1, R#result_packet.rows) end),
            lager:info("Parsing takes ~B...", [T2]),
            Result;
        R when is_record(R, error_packet)  ->
            case R#error_packet.code =:= ?TABLE_DOES_NOT_EXISTS of
                true  -> [];
                false -> throw_error(R)
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local functions:

execute(Query) ->
    try emysql:execute(?MODULE, Query, 1000*60)
    catch
        exit:connection_lock_timeout ->
            lager:info("connection lock timed out, retrying query", []),
            execute(Query);
        exit:Err -> {error, Err}
    end.

throw_error(R) when is_record(R, error_packet) ->
    exit({error, {R#error_packet.code, R#error_packet.msg}}).

parse_bar([Time, Open, High, Low, Close, Volume]) ->
    {trade_utils:to_datetime(Time), Open, High, Low, Close, Volume}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_tablename(Symbol, Period) when is_integer(Period) ->
    {ID, _, _, _} = get_symbol_info(Symbol),
    iolist_to_binary(io_lib:format("history_~B_~B", [ID, Period])).

table_exists(Name) ->
    QueryBin = <<"SHOW TABLES LIKE '", Name/binary, "'">>,
    case execute(QueryBin) of
        R when is_record(R, error_packet)  -> throw_error(R);
        R when is_record(R, result_packet) -> length(R#result_packet.rows) > 0
    end.

create_table(Name) ->
    QueryBin =
    <<"CREATE TABLE `", Name/binary, "` ("
      "`Timestamp` INTEGER UNSIGNED NOT NULL,"
      "`Open` DOUBLE NOT NULL,"
      "`High` DOUBLE NOT NULL,"
      "`Low` DOUBLE NOT NULL,"
      "`Close` DOUBLE NOT NULL,"
      "`Volume` DOUBLE NOT NULL,"
      "PRIMARY KEY (`Timestamp`)"
      ") ENGINE=MyISAM DEFAULT CHARSET=utf8;">>,
    case execute(QueryBin) of
        R when is_record(R, ok_packet)    -> ok;
        R when is_record(R, error_packet) ->
            case R#error_packet.code =:= ?TABLE_ALREADY_EXISTS of
                true  -> ok;
                false -> throw_error(R)
            end
    end.

drop_table(Name) ->
    QueryBin = <<"DROP TABLE ", Name/binary>>,
    case execute(QueryBin) of
        R when is_record(R, ok_packet)    -> ok;
        R when is_record(R, error_packet) ->
            case R#error_packet.code =:= ?UNKNOWN_TABLE of
                true  -> ok;
                false -> throw_error(R)
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_import_query(Ticks) when is_list(Ticks) -> build_import_query(Ticks, []);
build_import_query({Time, Open, High, Low, Close, Volume}) ->
    Args = [trade_utils:to_unixtime(Time), Open, High, Low, Close, Volume],
    io_lib:format("(~B,~w,~w,~w,~w,~w)", Args).

build_import_query([Bar], Result) -> iolist_to_binary(lists:reverse([build_import_query(Bar) | Result]));
build_import_query([Bar|Tail], Result) -> build_import_query(Tail, [$,, build_import_query(Bar) | Result]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

to_unixtime({Year, Month, Day}) ->
    to_unixtime({{Year, Month, Day}, {0, 0, 0}});
to_unixtime({{Year, Month, Day}, {Hour, Min, Sec}}) ->
    trade_utils:to_unixtime({{Year, Month, Day}, {Hour, Min, Sec}}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%