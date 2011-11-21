-module(trade_terminal).
-behaviour(gen_server).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include("trade_terminal_state.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% gen_server callbacks:
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% -record(request, {name, args, callback}).
-record(login_info, {name, pass, host, port, limit, period, interval, history=[]}).
-record(state, {name, socket, strategies, strategies_pid, login_info, terminal=#terminal_state{}}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Name, Options) ->
    gen_server:start_link({local, Name}, ?MODULE, {Name, Options}, []).

stop(Pid) ->
    stop(Pid, normal).

stop(Pid, Reason) ->
    gen_server:call(Pid, {stop, Reason}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_state(Pid) ->
    gen_server:call(Pid, get_terminal_state).

get_candlekinds(Pid) ->
    State = get_state(Pid),
    State#terminal_state.candlekinds.

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

buy_order(Pid, Market, Security, Amount) ->
    new_order(Pid, buy, Market, Security, Amount).

sell_order(Pid, Market, Security, Amount) ->
    new_order(Pid, sell, Market, Security, Amount).

new_order(Pid, Mode, Market, Security, Amount) ->
    Terminal   = get_state(Pid),
    ClientID   = get_client_id(Terminal),
    SecurityID = get_security_id(Market, Security, Terminal),
    send_sync_request(Pid, neworder, [Mode, SecurityID, ClientID, Amount]).

cancel_order(Pid, TransactionID) ->
    send_sync_request(Pid, cancelorder, [TransactionID]).

get_history(Pid, Market, Security, Period, Bars) ->
    get_history(Pid, Market, Security, Period, Bars, true).

get_history(Pid, Market, Security, Period, Bars, New) ->
    get_history(Pid, Market, Security, Period, Bars, New, 10000).

get_history(Pid, Market, Security, Period, Bars, New, Timeout) ->
    Terminal    = get_state(Pid),
    PeriodID    = get_period_id(Period, Terminal),
    SecurityID  = get_security_id(Market, Security, Terminal),
    send_sync_request(Pid, gethistorydata, [SecurityID, PeriodID, Bars, New], Timeout).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send_sync_request(Pid, Request) ->
    send_sync_request(Pid, Request, []).

send_sync_request(Pid, Request, Args) ->
    send_sync_request(Pid, Request, Args, 5000).

send_sync_request(Pid, Request, Args, Timeout) ->
    gen_server:call(Pid, {sync_request, Request, Args}, Timeout).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init({Name, Options}) ->
%     process_flag(trap_exit, true),

    lager:info("Starting terminal '~p'...", [Name]),
    {ok, Acceptor} = open_acceptor(),
    {ok, _} = open_terminal(Acceptor),
    {ok, Socket}   = open_socket(Acceptor),

    State =
    #state{
        name=Name,
        socket=Socket,
        strategies   = proplists:get_value(strategies, Options),
        login_info=#login_info{
            name     = proplists:get_value(name, Options),
            pass     = proplists:get_value(pass, Options),
            host     = proplists:get_value(host, Options),
            port     = proplists:get_value(port, Options),
            limit    = proplists:get_value(login_limit, Options, 5),
            period   = proplists:get_value(login_period, Options, 60),
            interval = proplists:get_value(login_interval, Options, 5)
    }},

    lager:info("Terminal '~p' started: ~p", [Name, self()]),
    schedule_login(),
    {ok, State}.

handle_call({stop, Reason}, _, State) ->
    {stop, Reason, ok, State};

handle_call({sync_request, Request, Args}, _, State) ->
    {Result, NewState} = sync_request(Request, Args, State),
    {reply, Result, NewState};

handle_call(get_terminal_state, _, State=#state{terminal=Terminal}) ->
    {reply, Terminal, State};

handle_call(Data, {From, _}, State) ->
    lager:debug("Unexpected call received from ~p: ~p", [From, Data]),
    {reply, {error, invalid_request}, State}.

handle_cast(login, State=#state{name=Name, login_info=#login_info{name=Login, pass=Pass, host=Host, port=Port}}) ->
    lager:info("Terminal '~p': connecting...", [Name]),
    case sync_request(login, [Login, Pass, Host, Port], State) of
        {ok, NewState} ->
            lager:info("Terminal '~p': connected", [Name]),
            {noreply, start_strategies(NewState)};
        {{error, {str, Error}}, NewState} ->
            lager:error("Terminal '~p': failed to connect: \"~ts\"", [Name, Error]),
            {noreply, relogin(NewState)};
        {{error, Error}, NewState} ->
            lager:error("Terminal '~p': failed to connect: ~p", [Name, Error]),
            {noreply, relogin(NewState)}
    end;

handle_cast(Data, State) ->
    lager:debug("Unexpected cast received: ~p", [Data]),
    {noreply, State}.

handle_info({tcp, Socket, RawData}, State=#state{socket=Socket, terminal=Terminal}) ->
    Data = parse(RawData),
    lager:debug("Data received from socket: ~p", [Data]),
    {noreply, update_state(update_terminal(Data, Terminal), State)};

% handle_info({tcp_error, Socket, Error}, State=#state{socket=Socket}) ->
%     lager:debug("Terminal ~p: closed with reason ~p", [self(), Error]),
%     {stop, {shutdown, {error, Error}}, State};
%
% handle_info({tcp_closed, Socket}, State=#state{socket=Socket}) ->
%     lager:debug("Terminal ~p: closed", [self()]),
%     {stop, {shutdown, ssl_closed}, State};

handle_info(Data, State) ->
    lager:debug("Unexpected info received: ~p", [Data]),
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sync_request(Name, Args, State=#state{socket=Socket}) ->
    ok = send_request(make_request(Name, Args), Socket),
    read_response(Name, [], State).

read_response(Name, Acc, State=#state{socket=Socket, terminal=Terminal}) ->
    case recv_data(Socket) of
        {ok, RawData} ->
            Data = parse(RawData),
            lager:debug("Got data: ~p", [Data]),
            NewState = update_state(update_terminal(Data, Terminal), State),
            case handle_response(Name, Data, Acc) of
                noreply        -> read_response(Name, Acc, NewState);
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

update_state(NewTerminal, State=#state{name=Name, terminal=OldTerminal}) ->
    NewState  = State#state{terminal=NewTerminal},
    OldStatus = OldTerminal#terminal_state.server_status,
    NewStatus = NewTerminal#terminal_state.server_status,

    case {NewStatus#server_status.recover, OldStatus#server_status.recover} of
        {false, true} -> lager:info("Terminal '~p': recovering connection...", [Name]);
        _             -> ok
    end,

    case {NewStatus#server_status.connected, OldStatus#server_status.connected} of
        {false, true} ->
            lager:info("Terminal '~p': disconnected", [Name]),
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
    {reply, lists:reverse(lists:concat([Chunk|Acc]))};

handle_response(_, {error, [], [Error]}, _Acc) ->
    {reply, {error, {str, Error}}};

handle_response(_, {result, [{success, "false"}], [{message, [], [Error]}]}, _Acc) ->
    {reply, {error, {str, Error}}};

handle_response(_, {server_status, [{id, _}, {connected, "false"}], _}, _Acc) ->
    {reply, {error, not_connected}};

handle_response(_Op, _Data, _Acc) ->
%     lager:debug("Operation: ~p, skipped data: ~p", [_Op, _Data]),
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
                "<rqdelay>500</rqdelay><session_timeout>25</session_timeout><request_timeout>20</request_timeout>"
            "</command>",
    io_lib:format(Format, Args);

make_request(logout, _) ->
    "<command id='disconnect'/>";

make_request(server_status, _) ->
    "<command id='server_status'/>";

make_request(gethistorydata, Args) ->
    Format = "<command id='gethistorydata' secid='~B' period='~B' count='~B' reset='~p'/>",
    io_lib:format(Format, Args).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_client_id(#terminal_state{clients=[#client{id=ID}]}) -> ID;
get_client_id(#terminal_state{}) -> exit(invalid_client).

get_period_id(Period, #terminal_state{candlekinds=Candles}) ->
    case lists:keyfind(Period*60, 3, Candles) of
        #candlekind{id=ID} -> ID;
        false          -> exit(invalid_period)
    end.

get_security_id(Market, Security, #terminal_state{securities=Securities}) -> get_security_id(Market, Security, Securities);
get_security_id(Market, Security, Securities) when is_list(Securities) ->
    case lists:keytake(Security, 5, Securities) of
        {value, #security{secid=ID, market=Market}, _} -> ID;
        {value, #security{}, Rest} -> get_security_id(Market, Security, Rest);
        false -> exit(invalid_security)
    end.

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

relogin(State=#state{name=Name, login_info=LoginInfo=#login_info{
        limit=Limit, period=Period, interval=Interval, history=History}}) ->

    Now = trade_utils:universal_time(),
    case lists:filter(fun(Time) -> Time > Now - Period end, History) of
        NewHistory when length(NewHistory) < Limit ->
            schedule_login(Interval),
            lager:info("Terminal '~p': reconnecting in ~B seconds...", [Name, Interval]),
            State#state{login_info=LoginInfo#login_info{history=[Now|NewHistory]}};
        _ ->
            lager:error("Terminal '~p': reconnect limit exceeded", [Name]),
            exit(restart_limit_exceeded)
    end.

start_strategies(State=#state{name=Name, strategies=Strategies}) ->
    lager:info("Starting strategies for terminal '~p'...", [Name]),
    {ok, Pid} = trade_strategy_mgr:start_link(Name, Strategies),
    State#state{strategies_pid=Pid}.

stop_strategies(State=#state{name=Name, strategies_pid=Pid}) ->
    lager:info("Stopping strategies for terminal '~p'...", [Name]),
    exit(Pid, normal),
    State#state{strategies_pid=undefined}.
