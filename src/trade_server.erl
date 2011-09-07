-module(trade_server).
-behavior(gen_server).

-export([start_link/0, start_client/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-import(trade_utils, [symbol/1, period/1, time/1, close/1]).

-define(SERVER, global:whereis_name(?MODULE)).
-define(TCP_OPTIONS, [binary, {packet, 4}, {nodelay, true}, {reuseaddr, true}, {active, once}, {keepalive, true}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {socket, peername, counter=0}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, server, []).

start_client(Socket) ->
    {ok, Pid} = gen_server:start_link(?MODULE, {client, Socket}, []),
    ok = gen_tcp:controlling_process(Socket, Pid),
    ok = inet:setopts(Socket, ?TCP_OPTIONS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(server) ->
    {ok, Host}    = application:get_env(host),
    {ok, Port}    = application:get_env(port),
    ServerOptions = [{name, undefined}, {port, Port}, {ip, Host}, {loop, {?MODULE, start_client}}],
    error_logger:info_msg("accepting connections at ~s:~B~n", [Host, Port]),
    {ok, _} = mochiweb_socket_server:start_link(ServerOptions),
    {ok, undefined};

init({client, Socket}) ->
    Peername = trade_utils:peername(Socket),
    error_logger:info_msg("socket accepted: ~s~n", [Peername]),
    {ok, #state{socket=Socket, peername=Peername}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call(Data, _, State) ->
    error_logger:info_msg("call: ~p~n", [Data]),
    {noreply, State}.

handle_cast(Data, State) ->
    error_logger:info_msg("cast: ~p~n", [Data]),
    {noreply, State}.

handle_info({tcp, Socket, Data}, State=#state{socket=Socket}) ->
    handle_command(unpack_command(Data), State);

handle_info({tcp_closed, Socket}, State=#state{socket=Socket, peername=Peername}) ->
    error_logger:info_msg("socket closed: ~s~n", [Peername]),
    {stop, normal, State};

handle_info(Data, State) ->
    error_logger:info_msg("info: ~p~n", [Data]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

code_change(_, State, _) -> {ok, State}.
terminate(_, _) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_command({tick, TickInfo}, State=#state{socket=Socket, peername=Peername, counter=Counter}) ->
    error_logger:info_msg("~s [~B]: getting tick info: ~p~n", [Peername, Counter, TickInfo]),
    send_response(Socket, 0),
    ok = inet:setopts(Socket,[{active,once}]),
    {noreply, State#state{counter=Counter+1}};

handle_command({hist, {Symbol, Period, From, To}}, State=#state{socket=Socket, peername=Peername, counter=Counter}) ->
    error_logger:info_msg("~s [~B]: getting history for ~w~n", [Peername, Counter, {Symbol, Period, From, To}]),
    case trade_db:get_history(Symbol, Period, From, To) of
        undefined        -> send_response(Socket, 0);
        HistoryTickInfo  -> send_response(Socket, pack_command(HistoryTickInfo))
    end,
    ok = inet:setopts(Socket,[{active,once}]),
    {noreply, State#state{counter=Counter+1}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unpack_command(<<"t", Period:32/little, Time:32/little,
                Open:64/little-float, High:64/little-float,
                Low:64/little-float, Close:64/little-float,
                Vol:64/little-float, Symbol/binary>>) ->
    {tick, {Symbol, Period, Time, Open, High, Low, Close, Vol}};

unpack_command(<<"h", Period:32/little, From:32/little, To:32/little, Symbol/binary>>) ->
    {hist, {Symbol, Period, From, To}}.

send_response(Socket, Result) ->
    ok = gen_tcp:send(Socket, <<Result:64/little-float>>).

pack_command({_Symbol, _Period, Time, Open, High, Low, Close, Volume}) ->
    T = trade_utils:to_unixtime(Time),
    <<T:32/little, Open:64/little-float, High:64/little-float, Low:64/little-float,
        Close:64/little-float, Volume:64/little-float>>;

pack_command(List) when is_list(List)->
    iolist_to_binary(lists:map(fun(X) -> pack_command(X) end, List)).
