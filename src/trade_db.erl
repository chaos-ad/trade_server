-module(trade_db).
-behavior(gen_server).
-compile(export_all).

-include("trade_timeframes.hrl").
-include_lib("emysql/include/emysql.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-define(DUPLICATE_ENTRY, 1062).
-define(TABLE_ALREADY_EXISTS, 1050).

-define(SERVER, {global, ?MODULE}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

get_history(Symbol, Period, Timestamp) ->
    gen_server:call(?SERVER, {get_history, Symbol, Period, Timestamp}, infinity).

get_history(Symbol, Period, From, To) ->
    gen_server:call(?SERVER, {get_history, Symbol, Period, From, To}, infinity).

put_history(TickInfo) ->
    gen_server:call(?SERVER, {put_history, TickInfo}, infinity).

get_finam_symbol_code(Symbol) ->
    gen_server:call(?SERVER, {get_finam_symbol_code, Symbol}, infinity).

put_finam_symbol_code(Symbol, Code) ->
    gen_server:call(?SERVER, {put_finam_symbol_code, Symbol, Code}, infinity).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
    process_flag(trap_exit, true),

    {ok, Options} = application:get_env(database),
    Port = proplists:get_value(port, Options, 3306),
    Host = proplists:get_value(host, Options, "localhost"),
    User = proplists:get_value(user, Options, "trade"),
    Pass = proplists:get_value(pass, Options, "trade"),
    Name = proplists:get_value(name, Options, "trade"),

    error_logger:info_msg("connecting to database ~s:~B...~n", [Host, Port]),
    ok = emysql:add_pool(?MODULE, 1, User, Pass, Host, Port, Name, utf8),
    ok = init_database(),
    {ok, undefined}.

init_database() ->
    case file:list_dir("priv/db/") of
        {ok, Scripts} ->
            lists:foreach( fun(File) -> execute_script("priv/db/" ++ File) end, Scripts );
        {error, noent} ->
            ok
    end.

execute_script(File) ->
    case file:read_file(File) of
        {ok, Query} ->
            error_logger:info_msg("executing '~s'...~n", [File]),
            case execute(Query) of
                R when is_record(R, ok_packet) -> ok;
                R when is_record(R, error_packet) ->
                    case R#error_packet.code =:= ?TABLE_ALREADY_EXISTS of
                        true -> ok;
                        false -> throw(make_error(R))
                    end
            end;
        _ -> ok
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({put_finam_symbol_code, Symbol, Code}, _, State) ->
    Query = "INSERT INTO finam_symbols VALUES (~s, ~B)",
    QueryBin = iolist_to_binary(io_lib:format(Query, [emysql_util:quote(Symbol), Code])),
    case execute(QueryBin) of
        R when is_record(R, ok_packet)    -> {reply, ok, State};
        R when is_record(R, error_packet) -> {reply, make_error(R), State}
    end;

handle_call({get_finam_symbol_code, Symbol}, _, State) ->
    Query = "SELECT CODE FROM finam_symbols WHERE Name = ~s",
    QueryBin = iolist_to_binary(io_lib:format(Query, [emysql_util:quote(Symbol)])),
    case execute(QueryBin) of
        R when is_record(R, error_packet)  -> {reply, make_error(R), State};
        R when is_record(R, result_packet) ->
            case R#result_packet.rows of
                [[Code]] -> {reply, Code, State};
                []       -> {reply, undefined, State}
            end
    end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({put_history, TickInfo}, _, State) when is_tuple(TickInfo) ->
    QueryBin = iolist_to_binary(["INSERT INTO history VALUES ", prepare_insertion(TickInfo)]),
%     error_logger:info_msg("Query: ~s~n", [QueryBin]),
    case execute(QueryBin) of
        R when is_record(R, ok_packet)    -> {reply, ok, State};
        R when is_record(R, error_packet) -> {reply, make_error(R), State}
    end;

handle_call({put_history, []}, _, State) -> {reply, ok, State};
handle_call({put_history, List}, _, State) when is_list(List) ->
%     error_logger:info_msg("importing ~b bars...~n", [length(List)]),
    QueryBin = iolist_to_binary(["INSERT INTO history VALUES ", prepare_insertion(List)]),
%     error_logger:info_msg("Query: ~s~n", [QueryBin]),
    case execute(QueryBin) of
        R when is_record(R, ok_packet)    -> {reply, ok, State};
        R when is_record(R, error_packet) -> {reply, make_error(R), State}
    end;

handle_call({get_history, Symbol, Period, Time}, _, State) ->
    Args  = [emysql_util:quote(Symbol), period(Period), emysql_util:quote(timestamp(Time))],
    Query = "SELECT * FROM history WHERE Symbol = ~s AND Period = ~B AND Timestamp = ~s",
    QueryBin = iolist_to_binary(io_lib:format(Query, Args)),
%     error_logger:info_msg("Query: ~p~n", [QueryBin]),
    case execute(QueryBin) of
        R when is_record(R, error_packet)  -> {reply, make_error(R), State};
        R when is_record(R, result_packet) ->
            case R#result_packet.rows of
                [Info] -> {reply, parse_tick_info(Info), State};
                []     -> {reply, undefined, State}
            end
    end;

handle_call({get_history, Symbol, Period, From, To}, _, State) ->
    QuotedTo   = emysql_util:quote(timestamp(To)),
    QuotedFrom = emysql_util:quote(timestamp(From)),
    Args  = [emysql_util:quote(Symbol), period(Period), QuotedFrom, QuotedTo],
    Query = "SELECT * FROM history WHERE Symbol = ~s AND Period = ~B AND Timestamp >= ~s AND Timestamp <= ~s",
    QueryBin = iolist_to_binary(io_lib:format(Query, Args)),
%     error_logger:info_msg("Query: ~p~n", [QueryBin]),
    case execute(QueryBin) of
        R when is_record(R, error_packet)  -> {reply, make_error(R), State};
        R when is_record(R, result_packet) -> {reply, lists:map(fun parse_tick_info/1, R#result_packet.rows), State}
    end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call(_, _, State) -> {noreply, State}.
handle_cast(_, State) -> {noreply, State}.
handle_info(_, State) -> {noreply, State}.
code_change(_, State, _) -> {ok, State}.

terminate(Reason, _State) ->
    emysql:remove_pool(?MODULE),
    error_logger:info_msg("terminated with reason ~p", [Reason]),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local functions:

execute(Query) ->
    try emysql:execute(?MODULE, Query, 1000*60)
    catch
        exit:connection_lock_timeout ->
            error_logger:info_msg("connection lock timed out, retrying query", []),
            execute(Query);
        exit:Err -> {error, Err}
    end.

make_error(R) when is_record(R, error_packet) ->
    {error, {R#error_packet.code, R#error_packet.msg}}.

parse_tick_info([Symbol, Period, {datetime, Time}, Open, High, Low, Close, Volume]) ->
    {Symbol, Period, Time, Open, High, Low, Close, Volume}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prepare_insertion({Symbol, Period, Time, Open, High, Low, Close, Volume}) ->
    S = emysql_util:quote(Symbol),
    P = period(Period),
    T = emysql_util:quote(timestamp(Time)),
    lists:flatten(io_lib:format("~n\t(~s,~B,~s,~w,~w,~w,~w,~w)", [S, P, T, Open, High, Low, Close, Volume]));

prepare_insertion(List) when is_list(List) ->
    prepare_insertion(List, []).

prepare_insertion([], Result) -> lists:reverse(Result);
prepare_insertion([Info], Result) -> lists:reverse([prepare_insertion(Info)|Result]);
prepare_insertion([Info|Tail], Result) -> prepare_insertion(Tail, [$,, prepare_insertion(Info)|Result]).

timestamp(Timestamp) when is_integer(Timestamp) ->
    timestamp(trade_utils:to_datetime(Timestamp));
timestamp({Year, Month, Day}) ->
    timestamp({{Year, Month, Day}, {0, 0, 0}});
timestamp({{Year, Month, Day}, {Hour, Min, Sec}}) ->
    lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B", [Year, Month, Day, Hour, Min, Sec])).

period(min)     -> ?PERIOD_M1;
period(min1)    -> ?PERIOD_M1;
period(min5)    -> ?PERIOD_M5;
period(min15)   -> ?PERIOD_M15;
period(min30)   -> ?PERIOD_M30;
period(hour)    -> ?PERIOD_H1;
period(hour1)   -> ?PERIOD_H1;
period(hour4)   -> ?PERIOD_H4;
period(day)     -> ?PERIOD_D1;
period(week)    -> ?PERIOD_W1;
period(month)   -> ?PERIOD_MN1;
period(Period) when is_integer(Period) -> Period.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%