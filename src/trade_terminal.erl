-module(trade_terminal).
-behaviour(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% gen_server callbacks:
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% api:
-export([start_link/1, stop/1, stop/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {socket, terminal}).

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

    _Name = proplists:get_value(name, Options),
    _Pass = proplists:get_value(pass, Options),
    _Host = proplists:get_value(host, Options),
    _Port = proplists:get_value(port, Options),

    AcceptOpts = [
        binary,
        {packet, 4},
        {ip, {127,0,0,1}},
        {keepalive, true}, {nodelay, true}, {reuseaddr, true}, {backlog, 1}
    ],
    lager:debug("Opening acceptor..."),
    {ok, Acceptor} = gen_tcp:listen(0, AcceptOpts),
    {ok, AcceptorPort} = inet:port(Acceptor),
    lager:debug("Acceptor opened at port ~B", [AcceptorPort]),

    {ok, TermPath} = application:get_env(terminal),
    TermArgs = ["--host localhost --port " ++ integer_to_list(AcceptorPort)],
    lager:debug("Spawning terminal process: ~p with args ~p", [TermPath, TermArgs]),
    TermOpts = [binary, nouse_stdio, hide, exit_status, {args, TermArgs}],
    Terminal = erlang:open_port({spawn_executable, TermPath}, TermOpts),
    lager:debug("Terminal spawned successfully"),

    {ok, Socket} = gen_tcp:accept(Acceptor, 10000),
    {ok, #state{socket=Socket, terminal=Terminal}}.

handle_call({stop, Reason}, _, State) ->
    {stop, Reason, State};

handle_call(Data, From, State) ->
    lager:debug("Unexpected call received from ~p: ~p", [From, Data]),
    {reply, {error, invalid_request}, State}.

handle_cast(Data, State) ->
    lager:debug("Unexpected cast received: ~p", [Data]),
    {noreply, State}.


handle_info({tcp, Socket, Data}, State=#state{socket=Socket}) ->
    lager:debug("Data received from socket: ~p", [Data]),
    {noreply, State};

handle_info({tcp_closed, Socket}, State=#state{socket=Socket}) ->
    lager:debug("Terminal disconnected with reaspon: socket closed"),
    {stop, normal, State};

handle_info({tcp_error, Socket, Reason}, State=#state{socket=Socket}) ->
    lager:debug("Terminal disconnected with reaspon: ~p", [Reason]),
    {stop, Reason, State};

handle_info({tcp_error, Socket, Reason}, State=#state{socket=Socket}) ->
    lager:debug("Terminal disconnected with reaspon: ~p", [Reason]),
    {stop, Reason, State};

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
