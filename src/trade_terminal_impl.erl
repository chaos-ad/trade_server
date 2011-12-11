-module(trade_terminal_impl).
-behaviour(trade_terminal).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include("trade_terminal_state.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([handle_info/2]).
-export([stop/1, start/1, handle_request/3, get_terminal_state/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {account, socket, terminal=#terminal_state{}}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

stop(_) -> ok.

start({Account, Options}) ->

    {ok, Acceptor} = open_acceptor(),
    {ok,        _} = open_terminal(Acceptor),
    {ok,   Socket} = open_socket(Acceptor),

    Name = proplists:get_value(name, Options),
    Pass = proplists:get_value(pass, Options),
    Host = proplists:get_value(host, Options),
    Port = proplists:get_value(port, Options),

    lager:info("Logging in..."),
    case handle_request(login, [Name, Pass, Host, Port], #state{socket=Socket, account=Account}) of
        {ok, NewState} ->
            lager:info("Logged successfully"),
            {ok, NewState};
        {{error, Error}, _} when is_list(Error) ->
            lager:error("Login failed: '~ts'", [Error]),
            {error, login_failed};
        {{error, Error}, _} ->
            lager:error("Login failed: ~p", [Error]),
            {error, login_failed}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_terminal_state(#state{terminal=Terminal}) ->
    Terminal.

handle_info({tcp, Socket, RawData}, State=#state{socket=Socket, terminal=Terminal}) ->
    Data = parse(RawData),
    lager:debug("Data received from socket: ~p", [Data]),
    {noreply, State#state{terminal=update_terminal(Data, Terminal)}};

handle_info({tcp_error, Socket, Error}, State=#state{socket=Socket}) ->
    {stop, {shutdown, {error, Error}}, State};

handle_info({tcp_closed, Socket}, State=#state{socket=Socket}) ->
    {stop, {shutdown, tcp_closed}, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_request(neworder, [Mode, Security, Amount], State) ->
    ClientID   = get_client_id(State#state.terminal),
    SecurityID = get_security_id(Security, State#state.terminal),
    Request    = make_request(neworder, [Mode, SecurityID, ClientID, Amount]),
    send_request(Request, State#state.socket),
    read_response(neworder, [], State);

handle_request(gethistorydata, [Security, Period, Bars, New], State) ->
    PeriodID   = get_period_id(Period, State#state.terminal),
    SecurityID = get_security_id(Security, State#state.terminal),
    Request    = make_request(gethistorydata, [SecurityID, PeriodID, Bars, New]),
    send_request(Request, State#state.socket),
    read_response(gethistorydata, [], State);

handle_request(Name, Args, State) ->
    Request = make_request(Name, Args),
    send_request(Request, State#state.socket),
    read_response(Name, [], State).

read_response(Name, Result, State=#state{socket=Socket, terminal=Terminal}) ->
    case recv_data(Socket) of
        {ok, RawData} ->
            Data = parse(RawData),
            lager:debug("Got data: ~p", [Data]),
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
    gen_tcp:send(Socket, Request).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_terminal({result,[{success,_}],[]}, State=#terminal_state{}) ->
    State; %% Ignoring result

update_terminal({server_status, [{id, ID}, {connected, "true"}, {recover, "true"}], _}, State=#terminal_state{}) ->
    State#terminal_state{server_status=#server_status{id=list_to_integer(ID), connected=true, recover=true}};

update_terminal({server_status, [{id, ID}, {connected, "true"}], _}, State=#terminal_state{}) ->
    State#terminal_state{server_status=#server_status{id=list_to_integer(ID), connected=true, recover=false}};

update_terminal({server_status, [{id, ID}, {connected, "false"}], _}, State=#terminal_state{}) ->
    exit(disconnected),
    State#terminal_state{server_status=#server_status{id=list_to_integer(ID), connected=false, recover=false}};

update_terminal({server_status, [{connected, "error"}], _}, State=#terminal_state{}) ->
    %% exit(disconnected),
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
    {reply, {error, Error}};

handle_response(_, {result, [{success, "false"}], [{message, [], [Error]}]}, _Acc) ->
    {reply, {error, Error}};

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

get_client_id(#terminal_state{clients=[#client{id=ID}]}) -> ID;
get_client_id(#terminal_state{}) -> exit(invalid_client).

get_period_id(Period, #terminal_state{candlekinds=Candles}) ->
    case lists:keyfind(Period*60, 3, Candles) of
        #candlekind{id=ID} -> ID;
        false          -> exit(invalid_period)
    end.

get_security_id({Market, Security}, #terminal_state{securities=Securities}) ->
    get_security_id(Market, Security, Securities);

get_security_id(Security, #terminal_state{securities=Securities}) ->
    get_security_id(1, Security, Securities).

get_security_id(Market, Security, Securities) when is_list(Securities) ->
    case lists:keytake(Security, 5, Securities) of
        {value, #security{secid=ID, market=Market}, _} -> ID;
        {value, #security{}, Rest} -> get_security_id(Market, Security, Rest);
        false -> exit(invalid_security)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%