-module(trade_terminal).
-compile(export_all).

-behavior(gen_server).

-include("trade_terminal_state.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-export
([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(request, {name, args, callback}).
-record(accinfo, {name, login, pass, host, port, attempts=1}).
-record(state,   {socket, accinfo, request_queue=queue:new(), terminal = #terminal_state{}}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(LOGIN_ATTEMPTS, 10).
-define(LOGIN_TIMEOUT, 1000*60).
-define(ACQUIRE_ACCOUNT_TIMEOUT, 1000*60*5).
-define(SOCKET_OPTIONS, [binary, {packet, 4}, {active, true}, {nodelay, true}, {reuseaddr, true}, {keepalive, true}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start({ssl, Socket}) ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    ok = ssl:controlling_process(Socket, Pid),
    ok = gen_server:call(Pid, {set_socket, Socket}),
    {ok, Pid}.

close(Pid) ->
    gen_server:call(Pid, close).

get_state(Pid) ->
    gen_server:call(Pid, get_terminal_state).

get_markets(Pid) ->
    State = get_state(Pid),
    State#terminal_state.markets.

get_securities(Pid) ->
    State = get_state(Pid),
    State#terminal_state.securities.

get_positions(Pid) ->
    State = get_state(Pid),
    State#terminal_state.positions.

get_orders(Pid) ->
    State = get_state(Pid),
    State#terminal_state.orders.

get_trades(Pid) ->
    State = get_state(Pid),
    State#terminal_state.trades.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

login(Pid, Login, Pass, Host, Port) ->
    send_request(Pid, login, [Login, Pass, Host, Port], 10000).

logout(Pid) ->
    send_request(Pid, logout).

buy_order(Pid, Market, Security, Amount) ->
    send_request(Pid, neworder, [buy, Market, Security, Amount]).

sell_order(Pid, Market, Security, Amount) ->
    send_request(Pid, neworder, [sell, Market, Security, Amount]).

close_order(Pid, TransactionID) ->
    send_request(Pid, cancelorder, [TransactionID]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send_request(Pid, Request) ->
    send_request(Pid, Request, []).

send_request(Pid, Request, Args) ->
    send_request(Pid, Request, Args, 5000).

send_request(Pid, Request, Args, Timeout) ->
    gen_server:call(Pid, {request, Request, Args}, Timeout).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
    {ok, #state{}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({set_socket, Socket}, _, State) ->
    {ok, {IP, Port}} = ssl:peername(Socket),
    lager:info("Connection ~s:~B accepted as terminal ~p", [inet_parse:ntoa(IP), Port, self()]),
    ok = ssl:setopts(Socket, ?SOCKET_OPTIONS),
    ok = gen_server:cast(self(), acquire_account),
    {reply, ok, State#state{socket=Socket}};

handle_call({request, neworder, [Mode, Market, Security, Amount]}, From, State=#state{terminal=Terminal}) ->
    try
        ClientID   = get_client_id(Terminal),
        SecurityID = get_security_id(Market, Security, Terminal),
        {noreply, add_request(neworder, [Mode, SecurityID, ClientID, Amount], reply_to(From), State)}
    catch
        _:Error ->
            {reply, {error, Error}, State}
    end;

handle_call({request, Name, Args}, From, State) ->
    {noreply, add_request(Name, Args, reply_to(From), State)};

handle_call(close, _, State=#state{socket=Socket}) ->
    ssl:close(Socket),
    {stop, normal, ok, State};

handle_call(get_terminal_state, _, State=#state{terminal=Terminal}) ->
    {reply, Terminal, State};

handle_call(Something, _, State) ->
    lager:debug("Terminal ~p receives unexpected call: ~p", [self(), Something]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_cast(acquire_account, State) ->
%     lager:debug("Terminal ~p: acquiring account...", [self()]),
    case trade_terminal_manager:acquire_account() of
        {ok, {Name, {Login, Pass, Host, Port}}} ->
            gen_server:cast(self(), perform_login),
%             lager:debug("Terminal ~p: account ~p acquired", [self(), Name]),
            AccInfo = #accinfo{name=Name, login=Login, pass=Pass, host=Host, port=Port},
            {noreply, State#state{accinfo=AccInfo}};
        _Response ->
%             lager:debug("Terminal ~p: account acquiring failed: ~p", [self(), _Response]),
            timer:apply_after(?ACQUIRE_ACCOUNT_TIMEOUT, gen_server, cast, [self(), acquire_account]),
            {noreply, State}
    end;

handle_cast(perform_login, State=#state{accinfo=AccInfo}) ->
    #accinfo{name=Name, login=Login, pass=Pass, host=Host, port=Port, attempts=Attempts} = AccInfo,
    lager:info("Terminal ~p: logging as '~p', attempt #~B...", [self(), Name, Attempts]),
    NewState = add_request(login, [Login, Pass, Host, Port], fun handle_login/2, State),
    {noreply, NewState#state{accinfo=AccInfo#accinfo{attempts=Attempts+1}}};


handle_cast(Something, State) ->
    lager:debug("Terminal ~p receives unexpected cast: ~p", [self(), Something]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_info({ssl, Socket, Data}, State=#state{socket=Socket}) ->
%     lager:debug("Terminal ~p receives:~n~ts", [self(), Data]),
    {noreply, handle_data(parse(Data), State)};

handle_info({ssl_error, Socket, Error}, State=#state{socket=Socket}) ->
    lager:debug("Terminal ~p: closed with reason ~p", [self(), Error]),
    {stop, {shutdown, {error, Error}}, State};

handle_info({ssl_closed, Socket}, State=#state{socket=Socket}) ->
    lager:debug("Terminal ~p: closed", [self()]),
    {stop, {shutdown, ssl_closed}, State};

handle_info(Something, State) ->
    lager:debug("Terminal ~p receives unexpected info: ~p", [self(), Something]),
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(Reason, #state{request_queue=Queue}) ->
    Fun = fun(#request{callback=Callback}) -> erlang:apply(Callback, {error, {stop, Reason}}) end,
    lists:foreach( Fun, queue:to_list(Queue) ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_data(Data, State=#state{terminal=Terminal, request_queue=Queue}) ->
%     lager:debug("Terminal ~p receives:~n~p", [self(), Data]),
    NewTerminal = update_terminal_state(Data, Terminal),
    case queue:out(Queue) of
        {empty, Queue} ->
            State#state{terminal=NewTerminal};
        {{value, Request}, NewQueue} ->
            case handle_response(Request#request.name, Data) of
                noreply ->
                    State#state{terminal=NewTerminal};
                {reply, Reply} ->
                    NewState1 = State#state{terminal=NewTerminal, request_queue=NewQueue},
                    NewState2 = erlang:apply(Request#request.callback, [Reply, NewState1]),
                    ok = send_request(NewState2),
                    NewState2
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_terminal_state({result,[{success,_}],[]}, State=#terminal_state{}) ->
    State; %% Ignoring result

update_terminal_state({server_status, [{id, ID}, {connected, "true"}, {recover, "true"}], _}, State=#terminal_state{}) ->
    trade_terminal_manager:set_terminal_state(recovering),
    State#terminal_state{server_status=#server_status{id=list_to_integer(ID), connected=true, recover=true}};

update_terminal_state({server_status, [{id, ID}, {connected, "true"}], _}, State=#terminal_state{}) ->
    trade_terminal_manager:set_terminal_state(connected),
    State#terminal_state{server_status=#server_status{id=list_to_integer(ID), connected=true}};

update_terminal_state({server_status, [{id, ID}, {connected, "false"}], _}, State=#terminal_state{}) ->
    trade_terminal_manager:set_terminal_state(disconnected),
    State#terminal_state{server_status=#server_status{id=list_to_integer(ID), connected=false}};

update_terminal_state({server_status, [{connected, "error"}], _}, State=#terminal_state{}) ->
    trade_terminal_manager:set_terminal_state(disconnected),
    State#terminal_state{server_status=#server_status{connected=false}};

update_terminal_state({client, [{id,ID},{remove,"true"}], []}, State=#terminal_state{clients=Clients}) ->
    State#terminal_state{clients=lists:keydelete(ID, 2, Clients)};

update_terminal_state({client, [{id, ID},{remove,"false"}], Args}, State=#terminal_state{clients=Clients}) ->
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

update_terminal_state({markets,[],MarketList}, State=#terminal_state{}) ->
    State#terminal_state{markets=lists:map(fun make_record/1, MarketList)};

update_terminal_state({securities,[],SecurityList}, State=#terminal_state{securities=Securities}) ->
    NewSecurities = lists:map(fun make_record/1, SecurityList),
    UpdFn = fun(Sec, Result) ->  lists:keystore(Sec#security.secid, 2, Result, Sec) end,
    State#terminal_state{securities=lists:foldl(UpdFn, Securities, NewSecurities)};

update_terminal_state({candlekinds,[],CandleList}, State=#terminal_state{}) ->
    State#terminal_state{candles=lists:map(fun make_record/1, CandleList)};

update_terminal_state({positions,[],PositionList}, State=#terminal_state{positions=Positions}) ->
    NewPositions = lists:map(fun make_record/1, PositionList),
    State#terminal_state{positions=update_positions(NewPositions, Positions)};

update_terminal_state({overnight,[{status,Overnight}],[]}, State=#terminal_state{}) ->
    State#terminal_state{overnight=list_to_atom(Overnight)};

update_terminal_state({trades, [], TradeList}, State=#terminal_state{trades=Trades}) ->
    NewTrades = lists:map(fun make_record/1, TradeList),
    State#terminal_state{trades=update_trades(NewTrades, Trades)};

update_terminal_state({orders, [], OrderList}, State=#terminal_state{orders=Orders}) ->
    NewOrders = lists:map(fun make_record/1, OrderList),
    State#terminal_state{orders=update_orders(NewOrders, Orders)};

update_terminal_state(_Data, State) ->
%     lager:debug("Data ignored: ~p", [_Data]),
    State.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_response(_, {result, [{success, "false"}], [{message, [], [Error]}]}) ->
%     lager:debug("Error: '~ts'", [Error]),
    {reply, {error, {str, Error}}};

handle_response(login, {server_status, [{id, _}, {connected, "true"}, {recover, "true"}], _}) ->
    {reply, ok};

handle_response(login, {server_status, [{id, _}, {connected, "true"}], _}) ->
    {reply, ok};

handle_response(login, {server_status, [{id, _}, {connected, "false"}], _}) ->
    {reply, {error, not_connected}};

handle_response(login, {server_status, [{connected, "error"}], [Error]}) ->
    {reply, {error, {str, Error}}};

handle_response(logout, {result,[{success,"true"}],[]}) ->
    {reply, ok};

handle_response(neworder, {result,[{success,"true"},{transactionid,ID}],[]}) ->
    {reply, {ok, list_to_integer(ID)}};

handle_response(cancelorder, {result, [{success, "true"}], _}) ->
    {reply, ok};

handle_response(_Op, _Data) ->
%     lager:info("Operation: ~p, skipped data: ~p", [_Op, _Data]),
    noreply.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_login({error, Error}, State=#state{accinfo=AccInfo}) ->
    case Error of
        {str, Message} -> lager:error("Terminal ~p: login failed with message: '~ts'", [self(), Message]);
        _              -> lager:error("Terminal ~p: login failed with error: ~p", [self(), Error])
    end,
    case AccInfo#accinfo.attempts =< ?LOGIN_ATTEMPTS of
        true ->
            timer:apply_after(?LOGIN_TIMEOUT, gen_server, cast, [self(), perform_login]),
            State;
        false ->
            trade_terminal_manager:release_account(),
            timer:apply_after(?ACQUIRE_ACCOUNT_TIMEOUT, gen_server, cast, [self(), acquire_account]),
            State#state{accinfo=undefined}
    end;

handle_login(ok, State=#state{accinfo=AccInfo}) ->
    lager:info("Terminal ~p: logged as '~p'", [self(), AccInfo#accinfo.name]),
    State#state{accinfo=AccInfo#accinfo{attempts=0}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_request(Name, Args, Callback, State=#state{request_queue=Queue}) ->
    Request  = #request{name=Name, args=Args, callback=Callback},
    NewQueue = queue:in(Request, Queue),
    NewState = State#state{request_queue=NewQueue},
    case queue:is_empty(Queue) of
        true  -> ok = send_request(NewState);
        false -> ok
    end,
    NewState.

send_request(#state{socket=Socket, request_queue=Queue}) ->
    case queue:peek(Queue) of
        empty -> ok;
        {value, #request{name=Name, args=Args}} ->
            Request = make_request(Name, Args),
%             lager:debug("Terminal ~p sends:~n~ts", [self(), iolist_to_binary(Request)]),
            ssl:send(Socket, Request)
    end.

reply_to(To) ->
    fun(Reply, State) -> gen_server:reply(To, Reply), State end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_client_id(#terminal_state{clients=[#client{id=ID}]}) -> ID;
get_client_id(#terminal_state{}) -> exit(invalid_client).

get_security_id(Market, Security, #terminal_state{securities=Securities}) -> get_security_id(Market, Security, Securities);
get_security_id(Market, Security, Securities) when is_list(Securities) ->
    case lists:keytake(Security, 5, Securities) of
        {value, #security{secid=ID, market=Market}, _} -> ID;
        {value, #security{}, Rest} -> get_security_id(Market, Security, Rest);
        false -> exit(invalid_security)
    end.

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
    BuySell =
    case Mode of
        buy  -> "B";
        sell -> "S"
    end,
    io_lib:format(Format, [SecurityID, ClientID, Amount, BuySell]);

make_request(cancelorder, Args) ->
    Format = "<command id='cancelorder'><transactionid>~B</transactionid></command>",
    io_lib:format(Format, Args);

make_request(login, Args) ->
    Format= "<command id='connect'>"
                "<login>~s</login><password>~s</password><host>~s</host><port>~B</port>"
                "<logsdir>./logs/</logsdir><loglevel>0</loglevel>"
                "<rqdelay>500</rqdelay><session_timeout>25</session_timeout><request_timeout>20</request_timeout>"
            "</command>",
    io_lib:format(Format, Args);

make_request(logout, _) -> "<command id='disconnect'/>";
make_request(server_status, _) ->"<command id='server_status'/>".

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
    #candle{id=list_to_integer(ID), period=list_to_integer(Period), name=Name};

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
