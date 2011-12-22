-module(trade_terminal).
-behaviour(gen_server).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include("trade_terminal_state.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% gen_server callbacks:
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(login_info, {name, pass, host, port, errors=0, attempts, interval}).
-record(state, {account, socket, terminal=#terminal_state{}, login_info=#login_info{}}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Account, Options) ->
    gen_server:start_link({local, Account}, ?MODULE, {Account, Options}, []).

stop(Pid) ->
    stop(Pid, normal).

stop(Pid, Reason) ->
    gen_server:call(Pid, {stop, Reason}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_state(Pid) ->
    gen_server:call(Pid, get_terminal_state).

get_status(Pid) ->
    trade_terminal_state:get_status(get_state(Pid)).

get_overnight(Pid) ->
    trade_terminal_state:get_overnight(get_state(Pid)).

get_client(Pid, ClientID) ->
    trade_terminal_state:get_client(ClientID, get_state(Pid)).

get_markets(Pid) ->
    trade_terminal_state:get_markets(get_state(Pid)).

get_candlekinds(Pid) ->
    trade_terminal_state:get_candlekinds(get_state(Pid)).

get_securities(Pid) ->
    trade_terminal_state:get_securities(get_state(Pid)).

get_security(Pid, Security) ->
    State = get_state(Pid),
    SecID = trade_terminal_state:get_security_id(Security, State),
    trade_terminal_state:get_security(SecID, State).

get_security_id(Pid, Security) ->
    trade_terminal_state:get_security_id(Security, get_state(Pid)).

get_positions(Pid) ->
    trade_terminal_state:get_positions(get_state(Pid)).

get_position(Pid, Security) ->
    State = get_state(Pid),
    SecID = trade_terminal_state:get_security_id(Security, State),
    trade_terminal_state:get_position(SecID, State).

get_position_lots(Pid, Security) ->
    State = get_state(Pid),
    SecID = trade_terminal_state:get_security_id(Security, State),
    trade_terminal_state:get_position_lots(SecID, State).

get_money(Pid) ->
    trade_terminal_state:get_money(get_state(Pid)).

get_money_position(Pid) ->
    trade_terminal_state:get_money_position(get_state(Pid)).

get_orders(Pid) ->
    trade_terminal_state:get_orders(get_state(Pid)).

get_order(Pid, OrderID) ->
    trade_terminal_state:get_order(OrderID, get_state(Pid)).

get_trades(Pid) ->
    trade_terminal_state:get_trades(get_state(Pid)).

get_trade(Pid, Security) ->
    State = get_state(Pid),
    SecID = trade_terminal_state:get_security_id(Security, State),
    trade_terminal_state:get_trade(SecID, State).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

buy_order(Pid, Security, Amount) ->
    new_order(Pid, buy, Security, Amount).

sell_order(Pid, Security, Amount) ->
    new_order(Pid, sell, Security, Amount).

new_order(Pid, Mode, Security, Amount) ->
    request(Pid, neworder, [Mode, Security, Amount]).

cancel_order(Pid, TransactionID) ->
    request(Pid, cancelorder, [TransactionID]).

get_history(Pid, Security, Period, Bars) ->
    get_history(Pid, Security, Period, Bars, true).

get_history(Pid, Security, Period, Bars, New) ->
    request(Pid, gethistorydata, [Security, Period, Bars, New]).

subscribe(Pid, Mode, Security) ->
    request(Pid, subscribe, [Mode, Security]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

request(Pid, Request) ->
    request(Pid, Request, []).

request(Pid, Request, Args) ->
    request(Pid, Request, Args, infinity).

request(Pid, Request, Args, Timeout) ->
    gen_server:call(Pid, {request, Request, Args}, Timeout).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init({Account, Options}) ->

    lager:info("Starting terminal '~p'...", [Account]),

    {ok, Acceptor} = open_acceptor(),
    {ok,        _} = open_terminal(Acceptor),
    {ok,   Socket} = open_socket(Acceptor),

    LoginInfo = #login_info{
        name  = proplists:get_value(name, Options),
        pass  = proplists:get_value(pass, Options),
        host  = proplists:get_value(host, Options),
        port  = proplists:get_value(port, Options),
        attempts = proplists:get_value(login_attempts, Options, 144), %% 12 hours of relogin
        interval = proplists:get_value(login_interval, Options, 300)  %% relogin every 5 minutess
    },
    gen_server:cast(self(), login),
    {ok, #state{socket=Socket, account=Account, login_info=LoginInfo}}.

handle_call({stop, Reason}, _, State) ->
    {stop, Reason, ok, State};

handle_call(get_terminal_state, _, State=#state{terminal=Terminal}) ->
    {reply, Terminal, State};

handle_call({request, Request, Args}, _, State=#state{}) ->
    {Result, NewState} = handle_request(Request, Args, State),
    {reply, Result, NewState};

handle_call(Data, {From, _}, State) ->
    lager:debug("Unexpected call from ~p received: ~p", [From, Data]),
    {noreply, State}.

handle_cast(login, State=#state{login_info=#login_info{attempts=Attempts, errors=Attempts}}) ->
    {stop, login_failed, State};

handle_cast(login, State=#state{account=Account, login_info=LoginInfo}) ->
    #login_info{name=Name, pass=Pass, host=Host, port=Port} = LoginInfo,
    #login_info{interval=Interval, attempts=Attempts, errors=Errors} = LoginInfo,
    lager:info("Terminal '~p': logging in...", [Account]),
    case handle_request(login, [Name, Pass, Host, Port], State) of
        {ok, NewState} ->
            true = gproc:add_local_name(Account),
            lager:info("Terminal '~p': logged successfully", [Account]),
            {noreply, NewState#state{login_info=LoginInfo#login_info{errors=0}}};
        {{error, Error}, _} when is_list(Error) ->
            lager:warning("Terminal '~p': login failed: '~ts'; retry in ~p seconds (~p left)", [Account, Error, Interval, Attempts-Errors]),
            timer:apply_after(Interval*1000, gen_server, cast, [self(), login]),
            {noreply, State#state{login_info=LoginInfo#login_info{errors=Errors+1}}};
        {{error, Error}, _} ->
            lager:warning("Terminal '~p': login failed: ~p; retry in ~p seconds (~p left)", [Account, Error, Interval, Attempts-Errors]),
            timer:apply_after(Interval*1000, gen_server, cast, [self(), login]),
            {noreply, State#state{login_info=LoginInfo#login_info{errors=Errors+1}}}
    end;

handle_cast(Data, State) ->
    lager:debug("Unexpected cast received: ~p", [Data]),
    {noreply, State}.

handle_info({tcp, Socket, RawData}, State=#state{socket=Socket, terminal=Terminal}) ->
    Data = trade_terminal_utils:parse(RawData),
    lager:debug("Data received: '~ts'", [RawData]),
%     lager:debug("Data parsed: ~p", [Data]),
    {noreply, State#state{terminal=update_terminal(Data, Terminal)}};

handle_info({tcp_error, Socket, Error}, State=#state{socket=Socket}) ->
    {stop, {shutdown, {error, Error}}, State};

handle_info({tcp_closed, Socket}, State=#state{socket=Socket}) ->
    {stop, {shutdown, tcp_closed}, State};

handle_info(Data, State) ->
    lager:debug("Unexpected info received: ~p", [Data]),
    {noreply, State}.

terminate(Reason, #state{account=Account}) ->
    lager:info("Terminal '~p' stopped: ~p", [Account, Reason]).

code_change(_, State, _) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_request(neworder, [Mode, Security, Amount], State=#state{terminal=Terminal}) ->
    ClientID   = trade_terminal_state:get_client_id(Terminal),
    SecurityID = trade_terminal_state:get_security_id(Security, Terminal),
    Request    = trade_terminal_utils:make_request(neworder, [Mode, SecurityID, ClientID, Amount]),
    send_request(Request, State#state.socket),
    read_response(neworder, [], State);

handle_request(gethistorydata, [Security, Period, Bars, New], State) ->
    PeriodID   = trade_terminal_state:get_period_id(Period, State#state.terminal),
    SecurityID = trade_terminal_state:get_security_id(Security, State#state.terminal),
    Request    = trade_terminal_utils:make_request(gethistorydata, [SecurityID, PeriodID, Bars, New]),
    send_request(Request, State#state.socket),
    read_response(gethistorydata, [], State);

handle_request(subscribe, [Mode, Security], State) ->
    SecurityID = trade_terminal_state:get_security_id(Security, State#state.terminal),
    Request    = trade_terminal_utils:make_request(subscribe, [{Mode, SecurityID}]),
    send_request(Request, State#state.socket),
    read_response(subscribe, [], State);

handle_request(Name, Args, State) ->
    Request = trade_terminal_utils:make_request(Name, Args),
    send_request(Request, State#state.socket),
    read_response(Name, [], State).

read_response(Name, Result, State=#state{socket=Socket, terminal=Terminal}) ->
    case recv_data(Socket) of
        {ok, RawData} ->
            Data = trade_terminal_utils:parse(RawData),
            lager:debug("Data received: '~ts'", [RawData]),
%             lager:debug("Data parsed: ~p", [Data]),
            NewState = State#state{terminal=update_terminal(Data, Terminal)},
            case handle_response(Name, Data, Result) of
                noreply        -> read_response(Name, Result, NewState);
                {more,  Reply} -> read_response(Name, Reply, NewState);
                {reply, Reply} -> {Reply, NewState}
            end;
        {error, Error} ->
            exit(Error)
    end.

recv_data(Socket) ->
    receive
        {tcp, Socket, Data} -> {ok, Data};
        {tcp_closed, Socket} -> {error, eof};
        {tcp_error, Socket, Reason} -> {error, Reason}
    end.

send_request(Request, Socket) ->
    lager:debug("Data sent: '~ts'", [Request]),
    gen_tcp:send(Socket, Request).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_terminal({result,[{success,_}],[]}, State=#terminal_state{}) ->
    State; %% Ignoring result

update_terminal({server_status, [{id, ID}, {connected, "true"}, {recover, "true"}], _}, State=#terminal_state{}) ->
    NewStatus = #server_status{id=list_to_integer(ID), connected=true, recover=true},
    trade_terminal_state:set_status(NewStatus, State);

update_terminal({server_status, [{id, ID}, {connected, "true"}], _}, State=#terminal_state{}) ->
    NewStatus = #server_status{id=list_to_integer(ID), connected=true, recover=false},
    trade_terminal_state:set_status(NewStatus, State);

update_terminal({server_status, [{id, ID}, {connected, "false"}], _}, State=#terminal_state{}) ->
    exit(disconnected),
    NewStatus = #server_status{id=list_to_integer(ID), connected=false, recover=false},
    trade_terminal_state:set_status(NewStatus, State);

update_terminal({server_status, [{connected, "error"}], _}, State=#terminal_state{}) ->
    %% exit(disconnected),
    trade_terminal_state:set_status(#server_status{connected=false, recover=false}, State);

update_terminal({client, [{id,ID},{remove,"true"}], []}, State=#terminal_state{}) ->
    trade_terminal_state:del_client(ID, State);

update_terminal({client, [{id, ID},{remove,"false"}], Args}, State=#terminal_state{}) ->
    Type = trade_terminal_utils:get_value(atom, type, Args),
    Curr = trade_terminal_utils:get_value(currency, Args),
    Client = #client{id=ID, type=Type, currency=Curr},
    case Client#client.type of
        spot ->
            trade_terminal_state:set_client(Client, State);
        leverage ->
            NewClient = Client#client{
                ml_intraday  = trade_terminal_utils:get_value(integer, ml_intraday, Args),
                ml_overnight = trade_terminal_utils:get_value(integer, ml_overnight, Args)
            },
            trade_terminal_state:set_client(NewClient, State);
        margin_level ->
            NewClient = Client#client{
                ml_call     = trade_terminal_utils:get_value(ml_call, Args),
                ml_close    = trade_terminal_utils:get_value(ml_close, Args),
                ml_restrict = trade_terminal_utils:get_value(ml_restrict, Args)
            },
            trade_terminal_state:set_client(NewClient, State)
    end;

update_terminal({markets,[],MarketList}, State=#terminal_state{}) ->
    trade_terminal_state:set_markets(lists:map(fun trade_terminal_utils:make_record/1, MarketList), State);

update_terminal({securities,[],SecurityList}, State=#terminal_state{}) ->
    NewSecurities = lists:map(fun trade_terminal_utils:make_record/1, SecurityList),
    lists:foldl(fun(X, S) -> trade_terminal_state:set_security(X, S) end, State, NewSecurities);

update_terminal({candlekinds,[],CandleList}, State=#terminal_state{}) ->
    trade_terminal_state:set_candlekinds(lists:map(fun trade_terminal_utils:make_record/1, CandleList), State);

update_terminal({positions,[],PositionList}, State=#terminal_state{}) ->
    NewPositions = lists:map(fun trade_terminal_utils:make_record/1, PositionList),
    lists:foldl(fun(X, S) -> trade_terminal_state:merge_positions(X, S) end, State, NewPositions);

update_terminal({overnight,[{status,Overnight}],[]}, State=#terminal_state{}) ->
    trade_terminal_state:set_overnight(list_to_atom(Overnight), State);

update_terminal({trades, [], TradeList}, State=#terminal_state{}) ->
    NewTrades = lists:map(fun trade_terminal_utils:make_record/1, TradeList),
    lists:foldl(fun(X, S) -> trade_terminal_state:set_trade(X, S) end, State, NewTrades);

update_terminal({orders, [], OrderList}, State=#terminal_state{}) ->
    NewOrders = lists:map(fun trade_terminal_utils:make_record/1, OrderList),
    lists:foldl(fun(X, S) -> trade_terminal_state:merge_order(X, S) end, State, NewOrders);

update_terminal(_Data, State) ->
%     lager:debug("Data ignored: ~p", [_Data]),
    State.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_response(login, {server_status, [{id, _}, {connected, "true"}, {recover, "true"}], _}, _Acc) ->
    {reply, ok};

handle_response(login, {server_status, [{id, _}, {connected, "true"}], _}, _Acc) ->
    {reply, ok};

handle_response(login, {server_status, [{id, _}, {connected, "false"}], _}, _Acc) ->
    {reply, {error, not_connected}};

handle_response(login, {server_status, [{connected, "error"}], [Error]}, _Acc) ->
    {reply, {error, Error}};

handle_response(logout, {result,[{success,"true"}],[]}, []) ->
    {reply, ok};

handle_response(neworder, {result,[{success,"true"},{transactionid,ID}],[]}, _Acc) ->
    {reply, {ok, list_to_integer(ID)}};

handle_response(cancelorder, {result, [{success, "true"}], _}, []) ->
    {reply, ok};

handle_response(gethistorydata, {candles, [{secid,_},{period,_},{status,"2"}], Candles}, Acc) ->
    FilteredCandles = lists:filter(fun({candle,_,_}) -> true; (_) -> false end, Candles),
    lager:debug("New history chunk: ~B bars", [length(FilteredCandles)]),
    Chunk = lists:map(fun trade_terminal_utils:make_record/1, FilteredCandles),
    {more, [Chunk|Acc]};

handle_response(gethistorydata, {candles, [{secid,_},{period,_},{status,S}], Candles}, Acc) ->
    FilteredCandles = lists:filter(fun({candle,_,_}) -> true; (_) -> false end, Candles),
    lager:debug("Last history chunk: ~B bars (status ~s)", [length(FilteredCandles), S]),
    Chunk = lists:map(fun trade_terminal_utils:make_record/1, FilteredCandles),
    {reply, {ok, lists:reverse(lists:concat([Chunk|Acc]))}};

handle_response(subscribe, {result,[{success,"true"}],[]}, _) ->
    {reply, ok};

handle_response(unsubscribe, {result,[{success,"true"}],[]}, _) ->
    {reply, ok};

handle_response(_, {error, [], [Error]}, _Acc) ->
    {reply, {error, Error}};

handle_response(_, {result, [{success, "false"}], [{message, [], [Error]}]}, _Acc) ->
    {reply, {error, Error}};

handle_response(_, {server_status, [{id, _}, {connected, "false"}], _}, _Acc) ->
    {reply, {error, not_connected}};

handle_response(_Op, _Data, _Acc) ->
%     lager:debug("Operation: ~p, skipped data: ~p", [_Op, _Data]),
    noreply.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

open_acceptor() ->
    lager:debug("Opening acceptor..."),
    {ok, LSocket} = gen_tcp:listen(0, [binary, {packet, 4}, {ip, {127,0,0,1}}]),
    {ok, Port} = inet:port(LSocket),
    lager:debug("Acceptor opened at port ~B", [Port]),
    {ok, {Port, LSocket}}.

open_terminal({Port, _}) ->
    {ok, TermPath} = application:get_env(terminal),
    lager:debug("Spawning external terminal process '~p'...", [TermPath]),
    TermArgs = ["--host localhost --port " ++ integer_to_list(Port)],
    TermOpts = [binary, nouse_stdio, hide, {args, TermArgs}],
    Terminal = erlang:open_port({spawn_executable, TermPath}, TermOpts),
    lager:debug("External terminal process spawned"),
    {ok, Terminal}.

open_socket({_, LSocket}) ->
    lager:debug("Linking to external terminal process..."),
    {ok, Result} = gen_tcp:accept(LSocket, infinity),
    lager:debug("External terminal process linked"),
    {ok, Result}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
