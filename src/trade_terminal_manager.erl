-module(trade_terminal_manager).
-compile(export_all).

% -include("trade_terminal.hrl").

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

% get_terminals() ->
%     gen_server:call(?SERVER, get_terminals).

init([]) ->
    process_flag(trap_exit, true),
    {ok, Opts} = application:get_env(terminals),
    Accounts   = to_accounts(proplists:get_value(accounts, Opts)),
    AcceptHost = proplists:get_value(accept_host, Opts),
    AcceptPort = proplists:get_value(accept_port, Opts),

    error_logger:info_msg("accepting terminal connections at ~s:~B~n", [AcceptHost, AcceptPort]),
    mochiweb_socket_server:start([
        {ip, AcceptHost},
        {port, AcceptPort},
        {name, trade_terminal_acceptor},
        {loop, fun(Socket) -> trade_terminal:start(Socket) end}
    ]),
    {ok, #state{terminals=[], accounts=Accounts}}.

handle_call({register_terminal, Pid}, _, State=#state{terminals=Terminals}) ->
    link(Pid),
    schedule_update(),
    {reply, ok, State#state{terminals=[#terminal{pid=Pid, name=undefined}|Terminals]}};

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
                    error_logger:info_msg("Terminal ~p: logging as ~p...~n", [Pid, Name]),
                    try trade_terminal:login(Pid, Login, Pass, Host, Port) of
                        {error, Error} ->
                            error_logger:info_msg("Terminal ~p: failed to login as ~p: ~p~n", [Pid, Name, Error]),
                            schedule_update(10000),
                            {noreply, State};
                        ok ->
                            true = register(Name, Pid),
                            schedule_update(1000),
                            error_logger:info_msg("Terminal ~p: logged as ~p~n", [Pid, Name]),
                            NewAccounts  = lists:keyreplace(false, 7, Accounts, A#account{logged=true}),
                            NewTerminals = lists:keyreplace(undefined, 3, Terminals, T#terminal{name=Name}),
                            {noreply, State#state{accounts=NewAccounts, terminals=NewTerminals}}
                    catch
                        _:Error ->
                            error_logger:info_msg("Terminal ~p: failed to login as ~p: ~p~n", [Pid, Name, Error]),
                            schedule_update(10000),
                            {noreply, State}
                    end
            end
    end;

handle_info({'EXIT', Pid, Reason}, State=#state{terminals=Terminals, accounts=Accounts}) ->
    case lists:keyfind(Pid, 2, Terminals) of
        false                -> {noreply, State};
        #terminal{name=Name} ->
            NewTerminals = lists:keydelete(Pid, 2, Terminals),
            NewState = State#state{terminals=NewTerminals},
            case Name of
                undefined -> {noreply, NewState};
                _         ->
                    error_logger:info_msg("Terminal ~p: logged off as ~p: ~p~n", [Pid, Name, Reason]),
                    schedule_update(1000),
                    Account     = lists:keyfind(Name, 2, Accounts),
                    NewAccounts = lists:keyreplace(Name, 2, Accounts, Account#account{logged=false}),
                    {noreply, NewState#state{accounts=NewAccounts}}
            end
    end;

handle_info(_, State) -> {noreply, State}.

code_changed(_, State, _) -> {noreply, State}.

terminate(_, _) ->
    error_logger:info_msg("Stopping socket acceptor...~n"),
    mochiweb_socket_server:stop(trade_terminal_acceptor).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

login_terminal(#terminal{pid=Pid}, #account{login=Login, pass=Pass, host=Host, port=Port, logged=false}) ->
    trade_terminal:login(Pid, Login, Pass, Host, Port).


to_accounts(List) when is_list(List) ->
    lists:map(fun to_account/1, List).

to_account({Name, Login, Pass, Host, Port}) ->
    #account{name=Name, login=Login, pass=Pass, host=Host, port=Port}.

schedule_update() ->
    erlang:send(self(), update_accounts).

schedule_update(Time) ->
    erlang:send_after(Time, self(), update_accounts).

