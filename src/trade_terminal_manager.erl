-module(trade_terminal_manager).
-compile(export_all).

%% TODO: Сделать очередь аккаунтов
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(SERVER, {global, ?MODULE}).
-define(UPDATE_TIME, 5000).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(terminal, {pid, name}).
-record(account,  {name, login, pass, host, port, logged=false}).
-record(state,    {terminals=[], accounts=[]}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    gen_server:start_link(?SERVER, ?MODULE, [], []).

% find_terminal(What) ->
%     gen_server:call(?SERVER, {find_terminal, What}).

register_terminal(Socket) ->
    gen_server:call(?SERVER, {register_terminal, Socket}).

get_terminals() ->
    gen_server:call(?SERVER, get_terminals).

get_accounts() ->
    gen_server:call(?SERVER, get_accounts).

add_account(Account) ->
    gen_server:call(?SERVER, {add_account, Account}).

del_account(Name) ->
    gen_server:call(?SERVER, {del_account, Name}).

init([]) ->
    lager:info("Initializing terminal manager..."),
    process_flag(trap_exit, true),
    {ok, Opts} = application:get_env(terminal_manager),
    Accounts   = to_accounts(proplists:get_value(accounts, Opts)),
    Host       = proplists:get_value(host, Opts),
    Port       = proplists:get_value(port, Opts),

    lager:info("Accepting terminal connections at ~s:~B~n", [Host, Port]),
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

    timer:send_interval(10000, update_accounts),
    timer:send_interval(1000*60*30, trade_some_shit),
    {ok, #state{terminals=[], accounts=Accounts}}.

handle_call({register_terminal, Pid}, _, State=#state{terminals=Terminals}) ->
    link(Pid),
    {reply, ok, State#state{terminals=[#terminal{pid=Pid, name=undefined}|Terminals]}};

handle_call(get_terminals, _, State=#state{terminals=Terminals}) ->
    {reply, Terminals, State};

handle_call(get_accounts, _, State=#state{accounts=Accounts}) ->
    {reply, lists:map(fun(Acc) -> from_account(Acc) end, Accounts), State};

handle_call({add_account, Account}, _, State=#state{accounts=Accounts}) ->
    {reply, ok, State#state{accounts=[to_account(Account)|Accounts]}};

handle_call({del_account, Name}, _, State=#state{accounts=Accounts}) ->
    {reply, ok, State#state{accounts=lists:keydelete(Name, 2, Accounts)}};

handle_call(_, _, State) -> {reply, {error, invalid_request}, State}.
handle_cast(_, State) -> {noreply, State}.

handle_info(update_accounts, State=#state{accounts=Accounts, terminals=Terminals}) ->
    case lists:keyfind(false, 7, Accounts) of
        false ->
            {noreply, State};
        A=#account{name=Name, login=Login, pass=Pass, host=Host, port=Port} ->
            case lists:keyfind(undefined, 3, Terminals) of
                false ->
                    {noreply, State};
                T=#terminal{pid=Pid} ->
                    lager:info("Terminal ~p: logging as ~p...~n", [Pid, Name]),
                    try trade_terminal:login(Pid, Login, Pass, Host, Port) of
                        {error, {str, Error}} ->
                            lager:info("Terminal ~p: failed to login as ~p: ~ts~n", [Pid, Name, Error]),
                            {noreply, State};
                        {error, Error} ->
                            lager:info("Terminal ~p: failed to login as ~p: ~p~n", [Pid, Name, Error]),
                            {noreply, State};
                        ok ->
                            true = register(Name, Pid),
                            lager:info("Terminal ~p: logged as ~p~n", [Pid, Name]),
                            NewAccounts  = lists:keyreplace(false, 7, Accounts, A#account{logged=true}),
                            NewTerminals = lists:keyreplace(undefined, 3, Terminals, T#terminal{name=Name}),
                            {noreply, State#state{accounts=NewAccounts, terminals=NewTerminals}}
                    catch
                        _:Error ->
                            lager:info("Terminal ~p: failed to login as ~p: ~p~n", [Pid, Name, Error]),
                            catch trade_terminal:close(Pid),
                            {noreply, State}
                    end
            end
    end;

handle_info(trade_some_shit, State=#state{terminals=Terminals}) ->
    TradeFn =
    fun(#terminal{name=undefined}) -> ok;
       (#terminal{name=Name, pid=Pid}) ->
        lager:info("Trading some shit using account '~p'...~n", [Name]),
        case random:uniform(2) =:= 1 of
            true  ->
                try trade_terminal:buy_order(Pid, 1, "AFLT", 1) of
                    {ok,   TransactionID} -> lager:info("Buying ok: ~p~n", [TransactionID]);
                    {error, {str, Error}} -> lager:error("Buying error: ~ts~n", [Error]);
                    {error,       Error } -> lager:error("Buying error: ~p~n", [Error])
                catch
                    _:Exception -> lager:error("Buying exception: ~p~n", [Exception])
                end;
            false ->
                try trade_terminal:sell_order(Pid, 1, "AFLT", 1) of
                    {ok,   TransactionID} -> lager:info("Selling ok: ~p~n", [TransactionID]);
                    {error, {str, Error}} -> lager:error("Selling error: ~ts~n", [Error]);
                    {error,       Error } -> lager:error("Selling error: ~p~n", [Error])
                catch
                    _:Exception -> lager:error("Selling exception: ~p~n", [Exception])
                end
        end
    end,
    lists:foreach(TradeFn, Terminals),
    {noreply, State};

handle_info({'EXIT', Pid, Reason}, State=#state{terminals=Terminals, accounts=Accounts}) ->
    case lists:keyfind(Pid, 2, Terminals) of
        false                -> {noreply, State};
        #terminal{name=Name} ->
            NewTerminals = lists:keydelete(Pid, 2, Terminals),
            NewState = State#state{terminals=NewTerminals},
            case Name of
                undefined ->
                    lager:info("Terminal ~p: logged off: ~p~n", [Pid, Reason]),
                    {noreply, NewState};
                _         ->
                    lager:info("Terminal ~p: logged off as ~p: ~p~n", [Pid, Name, Reason]),
                    case lists:keyfind(Name, 2, Accounts) of
                        false   -> {noreply, NewState};
                        Account ->
                            NewAccounts = lists:keyreplace(Name, 2, Accounts, Account#account{logged=false}),
                            {noreply, NewState#state{accounts=NewAccounts}}
                    end
            end
    end;

handle_info(_, State) -> {noreply, State}.

code_changed(_, State, _) -> {noreply, State}.

terminate(_, _) ->
    lager:info("Stopping socket acceptor...~n"),
    mochiweb_socket_server:stop(trade_terminal_acceptor).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

login_terminal(#terminal{pid=Pid}, #account{login=Login, pass=Pass, host=Host, port=Port, logged=false}) ->
    trade_terminal:login(Pid, Login, Pass, Host, Port).


to_accounts(List) when is_list(List) ->
    lists:map(fun to_account/1, List).

to_account({Name, Login, Pass, Host, Port}) ->
    #account{name=Name, login=Login, pass=Pass, host=Host, port=Port}.

from_account(#account{name=Name, login=Login, pass=Pass, host=Host, port=Port}) ->
    {Name, Login, Pass, Host, Port}.


