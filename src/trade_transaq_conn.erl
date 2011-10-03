-module(trade_transaq_conn).
-compile(export_all).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(TCP_OPTIONS, [binary, {packet, 4}, {nodelay, true}, {reuseaddr, true}, {active, true}, {keepalive, true}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {auth=false, socket, peername}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

logout(Pid) ->
    send(Pid, "<command id='disconnect'/>").

login(Pid, Login, Pass, Host, Port) ->
    Format =
    "<command id='connect'>"
        "<login>~s</login>"
        "<password>~s</password>"
        "<host>~s</host>"
        "<port>~B</port>"
        "<logsdir>./logs/</logsdir>"
        "<loglevel>0</loglevel>"
    "</command>",
    send(Pid, iolist_to_binary(io_lib:format(Format, [Login, Pass, Host, Port]))).

send_order(Pid) ->
    Cmd =
    "<command id='neworder'>"
        "<secid>6374</secid>"
        "<client>TCNN9956</client>"
        "<quantity>1</quantity>"
        "<buysell>B</buysell>"                  %% "('В' - покупка, или 'S' – продажа)
        "<bymarket/>"
        "<brokerref>примечание</brokerref>" %% (будет возвращено в составе структур order и trade)
        "<unfilled>ImmOrCancel</unfilled>" %% (другие возможные значения: CancelBalance, ImmOrCancel)
        "<usecredit/>"
        "<nosplit/>"
    "</command>",
    send(Pid, Cmd).

get_hist_data(Pid) ->
    send(Pid, "<command id='gethistorydata' secid='0' period='1' count='100000' reset='true'/>").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send(Pid, Data) ->
    gen_server:call(Pid, {send, Data}).

start(Socket) ->
    {ok, Pid} = gen_server:start(?MODULE, Socket, []),
    ok = gen_tcp:controlling_process(Socket, Pid),
    ok = inet:setopts(Socket, ?TCP_OPTIONS),
    {ok, Pid}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Socket) ->
    Peername = trade_utils:peername(Socket),
    error_logger:info_msg("transaq connection accepted: ~s: ~p~n", [Peername, self()]),
    {ok, #state{socket=Socket, peername=Peername}}.

handle_call({send, Data}, _, State=#state{socket=Socket, peername=Peername}) ->
    error_logger:info_msg("sending data to transaq connection ~s:~n~ts~n", [Peername, Data]),
    gen_tcp:send(Socket, Data),
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({tcp, Socket, NameBin}, State=#state{auth=false, socket=Socket, peername=Peername}) ->
    Name = erlang:binary_to_atom(NameBin, utf8),
    error_logger:info_msg("transaq connection ~s registered as ~p~n", [Peername, Name]),
    true = register(Name, self()),
    {noreply, State#state{auth=true}};

handle_info({tcp, Socket, Data}, State=#state{auth=true, socket=Socket, peername=Peername}) ->
    error_logger:info_msg("transaq connection ~s sends:~n~ts~n", [Peername, Data]),
    {noreply, State};

handle_info({tcp_closed, Socket}, State=#state{socket=Socket, peername=Peername}) ->
    error_logger:info_msg("transaq connection closed: ~s~n", [Peername]),
    {stop, normal, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, _) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_login(Pid) ->
    login(Pid, "TCNN9956", "VYDYD8", "195.128.78.60", 3939).
