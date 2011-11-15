-module(trade_terminal).
-behaviour(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% gen_server callbacks:
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% api:
-export([start_link/1, stop/1, stop/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {account, acceptor, terminal}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

stop(Pid) ->
    stop(Pid, normal).

stop(Pid, Reason) ->
    gen_server:call(Pid, {stop, Reason}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init({Name, Options}) ->
    process_flag(trap_exit, true),
    lager:info("Starting trade terminal '~p'...", [Name]),
    AccountOptions = proplists:get_value(account, Options),
    AcceptorOptions = proplists:get_value(acceptor, Options),
    TerminalOptions = proplists:get_value(terminal, Options),
    {ok, #state{account=AccountOptions,
                acceptor=start_acceptor(AcceptorOptions),
                terminal=start_terminal(TerminalOptions)}}.


handle_call({stop, Reason}, _, State) ->
    {stop, Reason, State};

handle_call(Data, From, State) ->
    lager:debug("Unexpected call received from ~p: ~p", [From, Data]),
    {reply, {error, invalid_request}, State}.

handle_cast(Data, State) ->
    lager:debug("Unexpected cast received: ~p", [Data]),
    {noreply, State}.

handle_info({'EXIT', Terminal, Reason}, State=#state{terminal=Terminal}) ->
    lager:debug("Terminal stopped with reaspon: ~p", [Reason]),
    {stop, Reason, State};

handle_info(Data, State) ->
    lager:debug("Unexpected info received: ~p", [Data]),
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_acceptor_options(Options) ->
    Self = self(),
    Host = proplists:get_value(host, Options),
    Port = proplists:get_value(port, Options),
    lager:info("Accepting terminal connection at ~s:~B", [Host, Port]),
    SSLOptions = make_ssl_options(proplists:get_value(ssl, Options, undefined)),
    Loop = fun(Socket) -> ok = gen_server:call(Self, {accept, Socket}) end,
    [{ip, Host}, {port, Port}, {loop, Loop}] ++ SSLOptions.

make_ssl_options(undefined) -> [];
make_ssl_options(SSLOptions) ->
    [{ssl, true},
     {ssl_opts, [
        {keyfile, proplists:get_value(keyfile, SSLOptions)},
        {certfile, proplists:get_value(certfile, SSLOptions)},
        {cacertfile, proplists:get_value(cacertfile, SSLOptions)},
        {verify, verify_peer},
        {fail_if_no_peer_cert, true}
    ]}].

start_acceptor(Options) ->
    AcceptorOptions = make_acceptor_options(Options),
    {ok, Pid} = mochiweb_socket_server:start(AcceptorOptions), Pid.

start_terminal(Options) ->
    Exe = proplists:get_value(exe, Options),
    lager:info("Spawning terminal process: ~p", [Exe]),
    Port = erlang:open_port({spawn_executable, Exe}, [binary, nouse_stdio, hide]),
    Port.

% send_login_request(Port, Args) ->
%     Format= "<command id='connect'>"
%                 "<login>~s</login><password>~s</password><host>~s</host><port>~B</port>"
%                 "<logsdir>./logs/</logsdir><loglevel>0</loglevel>"
%                 "<rqdelay>500</rqdelay><session_timeout>25</session_timeout><request_timeout>20</request_timeout>"
%             "</command>~n",
%     port_command(Port, io_lib:format(Format, Args)).
