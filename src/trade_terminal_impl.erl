-module(trade_terminal_impl).
-behaviour(trade_terminal).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include("trade_terminal_state.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([stop/1, start/1, handle_request/3, get_terminal_state/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(login_info, {name, pass, host, port, limit, period, interval, history=[]}).
-record(state, {account, socket, strategies, strategies_pid, login_info, terminal=#terminal_state{}}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

stop(_) -> ok.

start({Account, Options}) ->

    {ok, Acceptor} = open_acceptor(),
    {ok,        _} = open_terminal(Acceptor),
    {ok,   Socket} = open_socket(Acceptor),

    schedule_login(),

    {ok, #state{
        socket       = Socket,
        account      = Account,
        strategies   = proplists:get_value(strategies, Options, []),
        login_info   =
            #login_info{
                name     = proplists:get_value(name, Options),
                pass     = proplists:get_value(pass, Options),
                host     = proplists:get_value(host, Options),
                port     = proplists:get_value(port, Options),
                limit    = proplists:get_value(login_limit, Options, 5),
                period   = proplists:get_value(login_period, Options, 60),
                interval = proplists:get_value(login_interval, Options, 5)
            }
    }}.

get_terminal_state(#state{terminal=Terminal}) ->
    Terminal.

handle_request(Request, Args, State) ->
    request(Request, Args, State).

handle_call(get_terminal_state, _, State=#state{terminal=Terminal}) ->
    {reply, Terminal, State};

handle_call(Data, {From, _}, State) ->
    lager:debug("Unexpected call received from ~p: ~p", [From, Data]),
    {reply, {error, invalid_request}, State}.

handle_cast(login, State=#state{account=Account, login_info=LoginInfo}) ->
    lager:info("Terminal '~p': connecting...", [Account]),
    #login_info{name=Name, pass=Pass, host=Host, port=Port} = LoginInfo,
    case request(login, [Name, Pass, Host, Port], State) of
        {ok, NewState} ->
            lager:info("Terminal '~p': connected", [Account]),
            {noreply, start_strategies(NewState)};
        {{error, {str, Error}}, NewState} ->
            lager:error("Terminal '~p': failed to connect: \"~ts\"", [Account, Error]),
            {noreply, relogin(NewState)};
        {{error, Error}, NewState} ->
            lager:error("Terminal '~p': failed to connect: ~p", [Account, Error]),
            {noreply, relogin(NewState)}
    end;

handle_cast(Msg, State) ->
    lager:debug("Unexpected cast received: ~p", [Msg]),
    {noreply, State}.

handle_info({tcp, Socket, RawData}, State=#state{socket=Socket, terminal=Terminal}) ->
    Data = parse(RawData),
    lager:debug("Data received from socket: ~p", [Data]),
    {noreply, update_state(update_terminal(Data, Terminal), State)};

handle_info({tcp_error, Socket, Error}, State=#state{socket=Socket}) ->
    {stop, {shutdown, {error, Error}}, State};

handle_info({tcp_closed, Socket}, State=#state{socket=Socket}) ->
    {stop, {shutdown, tcp_closed}, State};

handle_info(Msg, State) ->
    lager:debug("Unexpected info received: ~p", [Msg]),
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

request(Name, Args, State=#state{socket=Socket}) ->
    ok = send_request(make_request(Name, Args), Socket),
    read_response(Name, [], State).

read_response(Name, Result, State=#state{socket=Socket, terminal=Terminal}) ->
    case recv_data(Socket) of
        {ok, RawData} ->
            Data = parse(RawData),
            lager:debug("Got data: ~p", [Data]),
            NewState = update_state(update_terminal(Data, Terminal), State),
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
    gen_tcp:send(Socket, Request).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_state(NewTerminal, State=#state{account=Account, terminal=OldTerminal}) ->
    NewState  = State#state{terminal=NewTerminal},
    OldStatus = OldTerminal#terminal_state.server_status,
    NewStatus = NewTerminal#terminal_state.server_status,

    case {NewStatus#server_status.recover, OldStatus#server_status.recover} of
        {false, true} -> lager:info("Terminal '~p': recovering connection...", [Account]);
        _             -> ok
    end,

    case {NewStatus#server_status.connected, OldStatus#server_status.connected} of
        {false, true} ->
            lager:info("Terminal '~p': disconnected", [Account]),
            relogin(stop_strategies(NewState));
        _ ->
            NewState
    end.

update_terminal({result,[{success,_}],[]}, State=#terminal_state{}) ->
    State; %% Ignoring result

update_terminal({server_status, [{id, ID}, {connected, "true"}, {recover, "true"}], _}, State=#terminal_state{}) ->
    State#terminal_state{server_status=#server_status{id=list_to_integer(ID), connected=true, recover=true}};

update_terminal({server_status, [{id, ID}, {connected, "true"}], _}, State=#terminal_state{}) ->
    State#terminal_state{server_status=#server_status{id=list_to_integer(ID), connected=true, recover=false}};

update_terminal({server_status, [{id, ID}, {connected, "false"}], _}, State=#terminal_state{}) ->
    State#terminal_state{server_status=#server_status{id=list_to_integer(ID), connected=false, recover=false}};

update_terminal({server_status, [{connected, "error"}], _}, State=#terminal_state{}) ->
    State#terminal_state{server_status=#server_status{connected=false, recover=false}};

update_terminal({client, [{id,ID},{remove,"true"}], []}, State=#terminal_state{clients=Clients}) ->
    State#terminal_state{clients=lists:keydelete(ID, 2, Clients)};

update_terminal({client, [{id, ID},{remove,"false"}], Args}, State=#terminal_state{clients=Clients}) ->
    Currency = get_value(currency, Args),
    Type     = get_value(atom, type, Args),
    Client   =
    case Type of
        spot ->
            #client{id=ID, type=Type, currency=Currency};
        leverage ->
            Intraday  = get_value(integer, ml_intraday, Args),
            Overnight = get_value(integer, ml_overnight, Args),
            #client{id=ID, type=Type, currency=Currency, ml_intraday=Intraday, ml_overnight=Overnight};
        margin_level ->
            Call     = get_value(ml_call, Args),
            Close    = get_value(ml_close, Args),
            Restrict = get_value(ml_restrict, Args),
            #client{id=ID, type=Type, currency=Currency, ml_call=Call, ml_close=Close, ml_restrict=Restrict}
    end,
    State#terminal_state{clients=[Client|lists:keydelete(ID, 2, Clients)]};

update_terminal({markets,[],MarketList}, State=#terminal_state{}) ->
    State#terminal_state{markets=lists:map(fun make_record/1, MarketList)};

update_terminal({securities,[],SecurityList}, State=#terminal_state{securities=Securities}) ->
    NewSecurities = lists:map(fun make_record/1, SecurityList),
    UpdFn = fun(Sec, Result) ->  lists:keystore(Sec#security.secid, 2, Result, Sec) end,
    State#terminal_state{securities=lists:foldl(UpdFn, Securities, NewSecurities)};

update_terminal({candlekinds,[],CandleList}, State=#terminal_state{}) ->
    State#terminal_state{candlekinds=lists:map(fun make_record/1, CandleList)};

update_terminal({positions,[],PositionList}, State=#terminal_state{positions=Positions}) ->
    NewPositions = lists:map(fun make_record/1, PositionList),
    State#terminal_state{positions=update_positions(NewPositions, Positions)};

update_terminal({overnight,[{status,Overnight}],[]}, State=#terminal_state{}) ->
    State#terminal_state{overnight=list_to_atom(Overnight)};

update_terminal({trades, [], TradeList}, State=#terminal_state{trades=Trades}) ->
    NewTrades = lists:map(fun make_record/1, TradeList),
    State#terminal_state{trades=update_trades(NewTrades, Trades)};

update_terminal({orders, [], OrderList}, State=#terminal_state{orders=Orders}) ->
    NewOrders = lists:map(fun make_record/1, OrderList),
    State#terminal_state{orders=update_orders(NewOrders, Orders)};

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
    {reply, {error, {str, Error}}};

handle_response(logout, {result,[{success,"true"}],[]}, []) ->
    {reply, ok};

handle_response(neworder, {result,[{success,"true"},{transactionid,ID}],[]}, _Acc) ->
    {reply, {ok, list_to_integer(ID)}};

handle_response(cancelorder, {result, [{success, "true"}], _}, []) ->
    {reply, ok};

handle_response(gethistorydata, {candles, [{secid,_},{period,_},{status,"2"}], Candles}, Acc) ->
    FilteredCandles = lists:filter(fun({candle,_,_}) -> true; (_) -> false end, Candles),
    lager:debug("New history chunk: ~B bars", [length(FilteredCandles)]),
    Chunk = lists:map(fun make_record/1, FilteredCandles),
    {more, [Chunk|Acc]};

handle_response(gethistorydata, {candles, [{secid,_},{period,_},{status,S}], Candles}, Acc) ->
    FilteredCandles = lists:filter(fun({candle,_,_}) -> true; (_) -> false end, Candles),
    lager:debug("Last history chunk: ~B bars (status ~s)", [length(FilteredCandles), S]),
    Chunk = lists:map(fun make_record/1, FilteredCandles),
    {reply, {ok, lists:reverse(lists:concat([Chunk|Acc]))}};

handle_response(subscribe, {result,[{success,"true"}],[]}, _) ->
    {reply, ok};

handle_response(unsubscribe, {result,[{success,"true"}],[]}, _) ->
    {reply, ok};

handle_response(_, {error, [], [Error]}, _Acc) ->
    {reply, {error, {str, Error}}};

handle_response(_, {result, [{success, "false"}], [{message, [], [Error]}]}, _Acc) ->
    {reply, {error, {str, Error}}};

handle_response(_, {server_status, [{id, _}, {connected, "false"}], _}, _Acc) ->
    {reply, {error, not_connected}};

handle_response(_Op, _Data, _Acc) ->
    lager:debug("Operation: ~p, skipped data: ~p", [_Op, _Data]),
    noreply.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

neworder_mode(buy) -> "B";
neworder_mode(sell) -> "S".

make_request(neworder, [Mode, SecurityID, ClientID, Amount]) ->
    Format =
    "<command id='neworder'>"
        "<secid>~B</secid>"
        "<client>~s</client>"
        "<quantity>~B</quantity>"
        "<buysell>~s</buysell>"
        "<unfilled>PutInQueue</unfilled>" %% CancelBalance, ImmOrCancel
        "<bymarket/>"
        "<nosplit/>"
    "</command>",
    io_lib:format(Format, [SecurityID, ClientID, Amount, neworder_mode(Mode)]);

make_request(cancelorder, Args) ->
    Format = "<command id='cancelorder'><transactionid>~B</transactionid></command>",
    io_lib:format(Format, Args);

make_request(login, Args) ->
    Format= "<command id='connect'>"
                "<login>~s</login><password>~s</password><host>~s</host><port>~B</port>"
                "<logsdir>./logs/</logsdir><loglevel>0</loglevel>"
                "<rqdelay>500</rqdelay><session_timeout>7</session_timeout><request_timeout>5</request_timeout>"
            "</command>",
    io_lib:format(Format, Args);

make_request(logout, _) ->
    "<command id='disconnect'/>";

make_request(server_status, _) ->
    "<command id='server_status'/>";

make_request(gethistorydata, Args) ->
    Format = "<command id='gethistorydata' secid='~B' period='~B' count='~B' reset='~p'/>",
    io_lib:format(Format, Args);

make_request(subscribe, Args) ->
    "<command id='subscribe'>" ++
        make_secids(quotes, Args) ++
        make_secids(alltrades, Args) ++
        make_secids(quotations, Args) ++
    "</command>";

make_request(unsubscribe, Args) ->
    "<command id='unsubscribe'>" ++
        make_secids(quotes, Args) ++
        make_secids(alltrades, Args) ++
        make_secids(quotations, Args) ++
    "</command>".

make_secid(ID) -> "<secid>" ++ integer_to_list(ID) ++ "</secid>".
make_secids(Mode, List) ->
    Secs = lists:map(fun({M, ID}) when M =:= Mode -> make_secid(ID); (_) -> [] end, List),
    lists:flatten(io_lib:format("<~p>~s</~p>", [Mode, Secs, Mode])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_position(Pos=#money_position{}, Positions) ->
    lists:keystore(money_position, 1, Positions, Pos);

update_position(NewPos=#sec_position{secid=ID}, Positions) ->
    case lists:keyfind(ID, 3, Positions) of
        false           ->
%             lager:debug("Adding new position: ~p", [NewPos]),
            [NewPos|Positions];
        OldPos ->
            List = lists:zip(tl(tuple_to_list(OldPos)), tl(tuple_to_list(NewPos))),
            Merged = lists:map(fun({X, undefined}) -> X; ({_, X}) -> X end, List),
            Result = list_to_tuple([sec_position|Merged]),
%             lager:debug("Old position: ~300p~nUpd position: ~300p~nNew position: ~300p", [OldPos, NewPos, Result]),
            lists:keyreplace(ID, 3, Positions, Result)
    end.

update_positions(NewPositions, Positions) ->
    lists:foldl(fun(Pos, PosList) -> update_position(Pos, PosList) end, Positions, NewPositions).

update_trade(Trade=#trade{secid=ID}, Trades) ->
    lists:keystore(ID, 2, Trades, Trade).

update_trades(NewTrades, Trades) ->
    lists:foldl(fun(Trade, TradeList) -> update_trade(Trade, TradeList) end, Trades, NewTrades).

update_order(NewOrder=#order{transactionid=ID}, Orders) ->
    case lists:keyfind(ID, 2, Orders) of
        false             ->
%             lager:debug("Adding new order: ~p", [NewOrder]),
            [NewOrder|Orders];
        OldOrder ->
            List = lists:zip(tl(tuple_to_list(OldOrder)), tl(tuple_to_list(NewOrder))),
            Merged = lists:map(fun({X, undefined}) -> X; ({_, X}) -> X end, List),
            Result = list_to_tuple([order|Merged]),
%             lager:debug("Old order: ~300p~nUpd order: ~300p~nNew order: ~300p", [OldOrder, NewOrder, Result]),
            lists:keyreplace(ID, 2, Orders, Result)
    end.

update_orders(NewOrders, Orders) ->
    lists:foldl(fun(Order, OrderList) -> update_order(Order, OrderList) end, Orders, NewOrders).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_record({candle,Attributes,[]}) ->
    #candle {
        date    = get_value(datetime, date, Attributes),
        open    = get_value(number, open, Attributes),
        close   = get_value(number, close, Attributes),
        high    = get_value(number, high, Attributes),
        low     = get_value(number, low, Attributes),
        volume  = get_value(number, volume, Attributes)
    };

make_record({order, Attributes, Args}) ->
    #order {
        transactionid   = get_value(integer, transactionid, Attributes),
        orderno         = get_value(integer, orderno, Args),
        secid           = get_value(integer, secid, Args),
        board           = get_value(board, Args),
        client          = get_value(client, Args),
        status          = get_value(status, Args),
        buysell         = get_value(buysell, Args),
        time            = get_value(time, Args),
        accepttime      = get_value(accepttime, Args),
        brokerref       = get_value(brokerref, Args),
        value           = get_value(number, value, Args),
        accruedint      = get_value(number, accruedint, Args),
        settlecode      = get_value(settlecode, Args),
        balance         = get_value(integer, balance, Args),
        price           = get_value(number, price, Args),
        quantity        = get_value(integer, quantity, Args),
        yield           = get_value(number, yield, Args),
        withdrawtime    = get_value(withdrawtime, Args),
        condition       = get_value(condition, Args),
        conditionvalue  = get_value(number, conditionvalue, Args),
        validafter      = get_value(validafter, Args),
        validbefore     = get_value(validbefore, Args),
        maxcomission    = get_value(integer, maxcomission, Args),
        result          = get_value(result, Args)
    };

make_record({trade, [], Args}) ->
    #trade{
        secid       = get_value(integer, secid, Args),
        tradeno     = get_value(integer, tradeno, Args),
        orderno     = get_value(integer, orderno, Args),
        board       = get_value(board, Args),
        client      = get_value(client, Args),
        buysell     = get_value(buysell, Args),
        time        = get_value(time, Args),
        brokerref   = get_value(brokerref, Args),
        value       = get_value(number, value, Args),
        comission   = get_value(number, comission, Args),
        price       = get_value(number, price, Args),
        quantity    = get_value(number, quantity, Args),
        yield       = get_value(number, yield, Args),
        currentpos  = get_value(number, currentpos, Args),
        accruedint  = get_value(number, accruedint, Args),
        tradetype   = get_value(tradetype, Args),
        settlecode  = get_value(settlecode, Args)
    };

make_record({money_position, [], Args}) ->
    #money_position{
        asset       = get_value(asset, Args),
        client      = get_value(client, Args),
        shortname   = get_value(shortname, Args),
        saldoin     = get_value(number, saldoin, Args),
        bought      = get_value(number, bought, Args),
        sold        = get_value(number, sold, Args),
        saldo       = get_value(number, saldo, Args),
        ordbuy      = get_value(number, ordbuy, Args),
        ordbuycond  = get_value(number, ordbuycond, Args),
        commission  = get_value(number, commission, Args)
    };

make_record({sec_position, [], Args}) ->
    #sec_position{
        client      = get_value(client, Args),
        secid       = get_value(integer, secid, Args),
        shortname   = get_value(shortname, Args),
        saldoin     = get_value(number, saldoin, Args),
        saldomin    = get_value(number, saldomin, Args),
        bought      = get_value(number, bought, Args),
        sold        = get_value(number, sold, Args),
        saldo       = get_value(number, saldo, Args),
        ordbuy      = get_value(number, ordbuy, Args),
        ordsell     = get_value(number, ordsell, Args)
    };

make_record({market,[{id,ID}],[Name]}) ->
    #market{id=list_to_integer(ID), name=Name};

make_record({kind,[],[{id,[],[ID]},{period,[],[Period]},{name,[],[Name]}]}) ->
    #candlekind{id=list_to_integer(ID), period=list_to_integer(Period), name=Name};

make_record({security, Attributes, Args}) ->

    {UseCredit, ByMarket, NoSplit, ImmOrCancel, CancelBalance} =
    case get_value(opmask, Args) of
        undefined            -> list_to_tuple(lists:duplicate(5, undefined));
        {opmask, OpMask, []} ->
            {get_value(usecredit,     OpMask) =:= "yes",
             get_value(bymarket,      OpMask) =:= "yes",
             get_value(nosplit,       OpMask) =:= "yes",
             get_value(immorcancel,   OpMask) =:= "yes",
             get_value(cancelbalance, OpMask) =:= "yes"}
    end,

    #security{  secid           = get_value(number, secid, Attributes),
                active          = get_value(atom, active, Attributes),
                sectype         = get_value(sectype, Args),
                seccode         = get_value(seccode, Args),
                market          = get_value(number, market, Args),
                shortname       = get_value(shortname, Args),
                decimals        = get_value(decimals, Args),
                minstep         = get_value(number, minstep, Args),
                lotsize         = get_value(number, lotsize, Args),
                point_cost      = get_value(number, point_cost, Args),
                usecredit       = UseCredit,
                bymarket        = ByMarket,
                nosplit         = NoSplit,
                immorcancel     = ImmOrCancel,
                cancelbalance   = CancelBalance
    }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_value(Name, Values) ->
    case lists:keyfind(Name, 1, Values) of
        false               -> undefined;
        {Name, Value}       -> Value;
        {Name, [], []}      -> undefined;
        {Name, [], [Value]} -> Value;
        {Name, [], [Value1, Value2]} -> Value1 ++ Value2;
        Other               -> Other
    end.

get_value(datetime, Name, Values) ->
    case get_value(Name, Values) of
        undefined -> undefined;
        Result    -> parse_datetime(Result)
    end;

get_value(atom, Name, Values) ->
    case get_value(Name, Values) of
        undefined -> undefined;
        Result    -> list_to_atom(Result)
    end;

get_value(integer, Name, Values) ->
    case get_value(Name, Values) of
        undefined -> undefined;
        Result    -> list_to_integer(Result)
    end;

get_value(number, Name, Values) ->
    case get_value(Name, Values) of
        undefined -> undefined;
        Result    -> list_to_number(Result)
    end.

list_to_number([C, $e | Value]) when C >= $0 andalso C =< $9 ->
    list_to_number([C, $., $0, $e | Value]);

list_to_number(Value) ->
    try list_to_float(Value)
    catch
        error:badarg -> list_to_integer(Value)
    end.

parse(Bin) when is_binary(Bin) ->
    case xmerl_scan:string(binary_to_list(Bin)) of
        {Result, []} -> xmerl_lib:simplify_element(Result);
        _            -> exit(parse_error)
    end.

parse_datetime(DateTime) ->
    {Date, Time} = lists:split(3, string:tokens(DateTime, [$., $:, $ ])),
    DateParsed = list_to_tuple(lists:reverse(lists:map(fun list_to_integer/1, Date))),
    TimeParsed = list_to_tuple(lists:map(fun list_to_integer/1, Time)),
    {DateParsed, TimeParsed}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

acceptor_opts() ->
    [binary,
    {packet, 4}, {ip, {127,0,0,1}},
    {keepalive, true}, {nodelay, true}, {reuseaddr, true}, {backlog, 1}].

open_acceptor() ->
    lager:debug("Opening acceptor..."),
    {ok, LSocket} = gen_tcp:listen(0, acceptor_opts()),
    {ok, Port} = inet:port(LSocket),
    lager:debug("Acceptor opened at port ~B", [Port]),
    {ok, {Port, LSocket}}.

open_terminal({Port, _}) ->
    {ok, TermPath} = application:get_env(terminal),
    TermArgs = ["--host localhost --port " ++ integer_to_list(Port)],
    lager:debug("Spawning terminal process: ~p with args ~p", [TermPath, TermArgs]),
    TermOpts = [binary, nouse_stdio, hide, {args, TermArgs}],
    Terminal = erlang:open_port({spawn_executable, TermPath}, TermOpts),
    lager:debug("Terminal spawned successfully"),
    {ok, Terminal}.

open_socket(Acceptor) -> open_socket(Acceptor, 5000).
open_socket({_, LSocket}, Timeout) -> gen_tcp:accept(LSocket, Timeout).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

schedule_login() ->
    gen_server:cast(self(), login).

schedule_login(Time) ->
    timer:apply_after(Time*1000, gen_server, cast, [self(), login]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

relogin(State=#state{account=Account, login_info=LoginInfo=#login_info{
        limit=Limit, period=Period, interval=Interval, history=History}}) ->

    Now = trade_utils:universal_time(),
    case lists:filter(fun(Time) -> Time > Now - Period end, History) of
        NewHistory when length(NewHistory) < Limit ->
            schedule_login(Interval),
            lager:info("Terminal '~p': reconnecting in ~B seconds...", [Account, Interval]),
            State#state{login_info=LoginInfo#login_info{history=[Now|NewHistory]}};
        _ ->
            lager:error("Terminal '~p': reconnect limit exceeded", [Account]),
            exit(restart_limit_exceeded)
    end.

start_strategies(State=#state{strategies=[]}) -> State;
start_strategies(State=#state{strategies=Strategies, account=Account}) ->
    lager:info("Starting strategies for terminal '~p'...", [Account]),
    {ok, Pid} = trade_strategy_mgr:start_link(Account, Strategies),
    State#state{strategies_pid=Pid}.

stop_strategies(State=#state{strategies_pid=undefined}) -> State;
stop_strategies(State=#state{strategies_pid=Pid, account=Account}) ->
    lager:info("Stopping strategies for terminal '~p'...", [Account]),
    exit(Pid, normal),
    State#state{strategies_pid=undefined}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
