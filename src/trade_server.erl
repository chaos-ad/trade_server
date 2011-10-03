-module(trade_server).
-behavior(gen_server).

-export([start_link/0, start_client/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-import(trade_utils, [symbol/1, period/1, time/1, close/1]).

-export([pack_history/1]).

-define(SERVER, global:whereis_name(?MODULE)).
-define(TCP_OPTIONS, [binary, {packet, 4}, {nodelay, true}, {reuseaddr, true}, {active, once}, {keepalive, true}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(GET_HISTORY_CMD, $h).

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
    Result =
    case handle_command(unpack_command(Data), State) of
        {noreply,      State} ->                          {noreply, State};
        {reply, Reply, State} -> send_data(Socket, Reply), {noreply, State}
    end,
    inet:setopts(Socket,[{active,once}, {packet, 4}]), Result;

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

handle_command({get_history, Symbol, Period, From, To}, State) ->
    History = trade_history:get_history(Symbol, Period, From, To),
    Args = [Symbol, Period, trade_utils:to_datestr(From), trade_utils:to_datestr(To), length(History)],
    error_logger:info_msg("Getting history for ~s (~B): ~s - ~s: ~B ticks", Args),
    {reply, pack_history(History), State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unpack_command(<<?GET_HISTORY_CMD, Period:32/little, From:32/little, To:32/little, Symbol/binary>>) ->
    {get_history, Symbol, Period, From, To}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send_data(Socket, Data) ->
    inet:setopts(Socket, [{packet, 0}]),
    gen_tcp:send(Socket, Data).

pack_tick({Time, Open, High, Low, Close, Volume}) ->
    <<Time:32/little,
      Open:64/little-float,  High:64/little-float,
       Low:64/little-float, Close:64/little-float,
    Volume:64/little-float>>.

pack_history([])        -> <<0:32/little>>;
pack_history(undefined) -> <<0:32/little>>;
pack_history(List) when is_list(List)->
    iolist_to_binary(lists:map(fun pack_tick/1, List)).
