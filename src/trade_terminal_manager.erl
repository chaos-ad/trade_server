-module(trade_terminal_manager).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(SERVER, {global, ?MODULE}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(NAME,     2).
-define(OPTIONS,  3).
-define(TERMINAL, 4).
-define(STATE,    5).
-record(account, {name, options, terminal=undefined, state=undefined}).
-record(state, {accounts}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

get_accounts() ->
    gen_server:call(?SERVER, get_accounts).

add_account(AccountInfo) ->
    gen_server:call(?SERVER, {add_account, AccountInfo}).

acquire_account() ->
    gen_server:call(?SERVER, acquire_account).

release_account() ->
    gen_server:call(?SERVER, release_account).

set_terminal_state(TerminalState) ->
    gen_server:call(?SERVER, {set_terminal_state, TerminalState}).

init([]) ->
    process_flag(trap_exit, true),
    lager:info("Initializing terminal manager..."),
    {ok, Opts}  = application:get_env(terminal_manager),
    AccountList = proplists:get_value(accounts, Opts),
    Host        = proplists:get_value(host, Opts),
    Port        = proplists:get_value(port, Opts),

    lager:info("Accepting terminal connections at ~s:~B", [Host, Port]),
    mochiweb_socket_server:start([
        {ip, Host},
        {port, Port},
        {name, trade_terminal_acceptor},
        {loop, fun(Socket) -> trade_terminal:start(Socket) end},
        {ssl, true},
        {ssl_opts, [
            {keyfile, proplists:get_value(keyfile, Opts)},
            {certfile, proplists:get_value(certfile, Opts)},
            {cacertfile, proplists:get_value(cacertfile, Opts)},
            {verify, verify_peer},
            {fail_if_no_peer_cert, true}
        ]}
    ]),

    {ok, #state{accounts=init_accounts(AccountList)}}.

init_accounts(AccountList) -> init_accounts(AccountList, []).
init_accounts([], Accounts) -> lists:reverse(Accounts);
init_accounts([AccountInfo|Tail], Accounts) ->
    Account = init_account(AccountInfo),
    case lists:keyfind(Account#account.name, ?NAME, Accounts) of
        true  -> erlang:exit(duplicate_accounts);
        false -> init_accounts(Tail, [Account|Accounts])
    end.

init_account({Name, Options}) -> #account{name=Name, options=Options}.

handle_call(get_accounts, _, State=#state{accounts=Accounts}) ->
    {reply, Accounts, State};

handle_call({add_account, AccountInfo}, _, State=#state{accounts=Accounts}) ->
    NewAccounts = init_accounts([AccountInfo], Accounts),
    {reply, ok, State#state{accounts=NewAccounts}};

handle_call(acquire_account, {Pid, _}, State=#state{accounts=Accounts}) ->
    case lists:keyfind(undefined, ?TERMINAL, Accounts) of
        false ->
            {reply, {error, no_free_accounts}, State};
        Account=#account{name=Name, options=Options} ->
            true = link(Pid),
            true = register(Name, Pid),
            NewAccount  = Account#account{terminal=Pid},
            NewAccounts = lists:keyreplace(Name, ?NAME, Accounts, NewAccount),
            lager:debug("Account '~p' was acquired by terminal ~p", [Name, Pid]),
            {reply, {ok, {Name, Options}}, State#state{accounts=NewAccounts}}
    end;

handle_call(release_account, {Pid, _}, State=#state{accounts=Accounts}) ->
    case lists:keyfind(Pid, ?TERMINAL, Accounts) of
        false ->
            {reply, {error, not_acquired}, State};
        Account=#account{name=Name} ->
            true = unlink(Pid),
            catch unregister(Name),
            NewAccount  = Account#account{terminal=undefined, state=undefined},
            NewAccounts = lists:keyreplace(Name, ?NAME, Accounts, NewAccount),
            lager:debug("Account '~p' was released by terminal ~p", [Name, Pid]),
            {reply, ok, State#state{accounts=NewAccounts}}
    end;

handle_call({set_terminal_state, TerminalState}, {Pid, _}, State=#state{accounts=Accounts}) ->
    case lists:keyfind(Pid, ?TERMINAL, Accounts) of
        false ->
            {reply, {error, not_acquired}, State};
        Account=#account{name=Name} ->
            NewAccount  = Account#account{state=TerminalState},
            NewAccounts = lists:keyreplace(Name, ?NAME, Accounts, NewAccount),
            lager:debug("Terminal ~p becomes ~p", [Pid, TerminalState]),
            {reply, ok, State#state{accounts=NewAccounts}}
    end;

handle_call(_, _, State) ->
    {reply, {error, invalid_request}, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, Reason}, State) ->
    case handle_call(release_account, {Pid, undefined}, State) of
        {reply, ok, NewState} ->
            {noreply, NewState};
        {reply, {error, not_acquired}, State} ->
            lager:debug("Unexpected exit signal received from ~p: ~p", [Pid, Reason]),
            {noreply, State}
    end;

handle_info(_, State) -> {noreply, State}.
code_changed(_, State, _) -> {noreply, State}.

terminate(Reason, _) ->
    lager:info("Stopping terminal manager with reason: ~p", [Reason]),
    mochiweb_socket_server:stop(trade_terminal_acceptor).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
