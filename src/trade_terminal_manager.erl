-module(trade_terminal_manager).
-compile(export_all).

%% TODO: Сделать очередь аккаунтов
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% -define(UPDATE_TIME, 5000).
-define(SERVER, global:whereis_name(?MODULE)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% -record(terminal, {pid, name}}
% -record(account,  {name, login, pass, host, port}).
-record(state, {logged_accounts=[], logged_terminals=[], free_accounts=[], free_terminals=[]}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

find_terminal(What) ->
    gen_server:call(?SERVER, {find_terminal, What}).

accept_terminal(Socket) ->
    gen_server:call(?SERVER, {accept_terminal, Socket}).

get_accounts(Mode) ->
    gen_server:call(?SERVER, {get_accounts, Mode}).

get_terminals(Mode) ->
    gen_server:call(?SERVER, {get_terminals, Mode}).

add_account(AccountInfo) ->
    gen_server:call(?SERVER, {add_account, AccountInfo}).

% del_account(AccountName) ->
%     gen_server:call(?SERVER, {del_account, AccountName}).

init([]) ->
    lager:info("Initializing terminal manager..."),
    process_flag(trap_exit, true),
    {ok, Opts} = application:get_env(terminal_manager),
    Accounts   = proplists:get_value(accounts, Opts),
    Host       = proplists:get_value(host, Opts),
    Port       = proplists:get_value(port, Opts),

    lager:info("Accepting terminal connections at ~s:~B", [Host, Port]),
    mochiweb_socket_server:start([
        {ip, Host},
        {port, Port},
        {name, trade_terminal_acceptor},
        {loop, fun(Socket) -> ?MODULE:accept_terminal(Socket) end},
        {ssl, true},
        {ssl_opts, [
            {keyfile, proplists:get_value(keyfile, Opts)},
            {certfile, proplists:get_value(certfile, Opts)},
            {cacertfile, proplists:get_value(cacertfile, Opts)},
            {verify, verify_peer},
            {fail_if_no_peer_cert, true}
        ]}
    ]),

    timer:send_interval(10000, update_terminals),

    {ok, #state{free_accounts=Accounts}}.

handle_call({accept_terminal, {ssl, Socket}}, _, State=#state{free_terminals=FreeTerminals}) ->
    {ok, {IP, Port}} = ssl:peername(Socket),
    lager:debug("Socket accepted: ~s:~B", [inet_parse:ntoa(IP), Port]),
    {ok, Terminal} = trade_terminal:start_link(Socket),
    {reply, ok, State#state{free_terminals=[{undefined, Terminal}|FreeTerminals]}};

handle_call({get_terminals, Mode}, _, State) ->
    {reply, get_accounts(Mode, State), State};

handle_call({get_accounts, Mode}, _, State) ->
    {reply, get_accounts(Mode, State), State};

handle_call({add_account, Account}, _, State) ->
    {reply, ok, add_account(free, Account, State)};

handle_call({find_terminal, {Where, What}}, _, State) ->
    {reply, find_terminal(Where, What, State), State};

handle_call({terminal_logged_in, Terminal, Account}, _, S0) ->
    true = register(element(1, Terminal), element(2, Terminal)),
    S1 = add_account(logged, Account, S0),
    S2 = add_terminal(logged, Terminal, S1),
    {reply, ok, S2};

handle_call({terminal_logged_out, Terminal, Account}, _, S0) ->
    true = unregister(element(1, Terminal)),
    S1 = del_account(logged, Account, S0),
    S2 = del_terminal(logged, Terminal, S1),
    S3 = add_account(free, Account, S2),
    S4 = add_terminal(free, Terminal, S3),
    {reply, ok, S4};

handle_call({terminal_login_failed, Terminal, Account, Error}, _, S0) ->
    print_error(Terminal, Account, Error),
    case Error of
        {fatal, _} ->
            catch trade_terminal:close(Terminal),
            S1 = add_account(free, Account, S0),
            {reply, ok, S1};
        {retry, _} ->
            S1 = add_account(free, Account, S0),
            S2 = add_terminal(free, Terminal, S1),
            {reply, ok, S2}
    end;

handle_call(_, _, State) -> {reply, {error, invalid_request}, State}.
handle_cast(_, State) -> {noreply, State}.

handle_info(update_terminals, S0=#state{free_accounts=[]}) -> {noreply, S0};
handle_info(update_terminals, S0=#state{free_terminals=[]}) -> {noreply, S0};
handle_info(update_terminals, S0=#state{free_accounts=FreeAccounts, free_terminals=FreeTerminals}) ->
    Account = lists:nth(random:uniform(length(FreeAccounts)), FreeAccounts),
    Terminal = lists:nth(random:uniform(length(FreeTerminals)), FreeTerminals),
    start_async_login(Terminal, Account),
    S1 = del_account(free, {name, element(1, Account)}, S0),
    S2 = del_terminal(free, {pid, element(2, Terminal)}, S1),
    {noreply, S2};

handle_info({'EXIT', Pid, Reason}, S0) ->
    case find_terminal(logged, {pid, Pid}, S0) of
        undefined ->
            case find_terminal(free, {pid, Pid}, S0) of
                undefined ->
                    lager:warning("Unexpected exit signal received from pid ~p", [Pid]),
                    {noreply, S0};
                _ ->
                    lager:info("Terminal ~p disconnected: ~p", [Pid, Reason]),
                    {noreply, del_terminal(free, {pid, Pid}, S0)}
            end;
        {Name, Pid} ->
            Account = find_account(logged, {name, Name}, S0),
            case Account =:= undefined of
                true  -> erlang:error(internal_error);
                false -> ok
            end,
            S1 = add_account(free, Account, S0),
            S2 = del_account(logged, {name, Name}, S1),
            S3 = del_terminal(logged, {name, Name}, S2),
            lager:info("Terminal ~p (logged as '~p'), disconnected: ~p", [Pid, Name, Reason]),
            {noreply, S3}
    end;

handle_info(_, State) -> {noreply, State}.

code_changed(_, State, _) -> {noreply, State}.

terminate(Reason, _) ->
    lager:info("Stopping terminal manager with reason: ~p", [Reason]),
    mochiweb_socket_server:stop(trade_terminal_acceptor).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_error(Terminal, {Name, _}, {retry, {str, Error}}) ->
    lager:info("Terminal ~p: error during login as '~p': ~ts", [Terminal, Name, Error]);
print_error(Terminal, {Name, _}, {retry, Error}) ->
    lager:info("Terminal ~p: error during login as '~p': ~p", [Terminal, Name, Error]);
print_error(Terminal, {Name, _}, {fatal, {str, Error}}) ->
    lager:info("Terminal ~p: fatal error during login as '~p': ~ts", [Terminal, Name, Error]);
print_error(Terminal, {Name, _}, {fatal, Error}) ->
    lager:info("Terminal ~p: fatal error during login as '~p': ~p", [Terminal, Name, Error]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_accounts(free, #state{free_accounts=Result}) -> Result;
get_accounts(logged, #state{logged_accounts=Result}) -> Result.

set_accounts(free, Accounts, State) -> State#state{free_accounts=Accounts};
set_accounts(logged, Accounts, State) -> State#state{logged_accounts=Accounts}.

add_account(Mode, Account, State) ->
    set_accounts(Mode, [Account|get_accounts(Mode, State)], State).
del_account(Mode, {name, Name}, State) ->
    set_accounts(Mode, lists:keydelete(Name, 1, get_accounts(Mode, State)), State);
del_account(Mode, {Name, _}, State) ->
    del_account(Mode, {name, Name}, State).

find_result(false) -> undefined;
find_result(Other) -> Other.

find_account(Mode, {name, Name}, State) ->
    find_result( lists:keyfind(Name, 1, get_accounts(Mode, State)) ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_terminals(free, #state{free_terminals=Result}) -> Result;
get_terminals(logged, #state{logged_terminals=Result}) -> Result.

set_terminals(free, Terminals, State) -> State#state{free_terminals=Terminals};
set_terminals(logged, Terminals, State) -> State#state{logged_terminals=Terminals}.

add_terminal(Mode, {Pid, Name}, State) ->
    set_terminals(Mode, [{Pid, Name}|get_terminals(Mode, State)], State).

del_terminal(Mode, {pid, Pid}, State) ->
    set_terminals(Mode, lists:keydelete(Pid, 2, get_terminals(Mode, State)), State);
del_terminal(Mode, {name, Name}, State) ->
    set_terminals(Mode, lists:keydelete(Name, 1, get_terminals(Mode, State)), State);
del_terminal(Mode, {_, Pid}, State) ->
    del_terminal(Mode, {pid, Pid}, State).

find_terminal(Mode, {pid, Name}, State) ->
    find_result( lists:keyfind(Name, 2, get_terminals(Mode, State)) );
find_terminal(Mode, {name, Name}, State) ->
    find_result( lists:keyfind(Name, 1, get_terminals(Mode, State)) ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_async_login(Terminal, {Name, Credentials}) ->
    lager:info("Terminal ~p: logging as '~p'...", [element(2, Terminal), Name]),
    spawn(fun() -> do_login(Terminal, {Name, Credentials}) end),
    ok.

do_login(Terminal, Account) ->
    {_, TerminalPid} = Terminal,
    {Name, {Login, Passwd, Host, Port}} = Account,
    try trade_terminal:login(TerminalPid, Login, Passwd, Host, Port) of
        ok                    ->
            lager:info("Terminal ~p: logging result: ~p", [TerminalPid, ok]),
            ok = gen_server:call(?SERVER, {terminal_logged_in, {Name, TerminalPid}, Account});
        {error, {str, Error}} ->
            lager:info("Terminal ~p: logging result: ~p", [TerminalPid, {error, {str, Error}}]),
            ok = gen_server:call(?SERVER, {terminal_login_failed, Terminal, Account, {retry, Error}});
        {error, Error}        ->
            lager:info("Terminal ~p: logging result: ~p", [TerminalPid, {error, Error}]),
            ok = gen_server:call(?SERVER, {terminal_login_failed, Terminal, Account, {fatal, Error}})
    catch
        _:Exception    ->
            lager:info("Terminal ~p: logging exception: ~p", [TerminalPid, {error, Exception}]),
            ok = gen_server:call(?SERVER, {terminal_login_failed, Terminal, Account, {fatal, Exception}})
    end.