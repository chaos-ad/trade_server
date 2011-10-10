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

-record(request, {name, args, from}).
-record(state,   {socket, endpoint, request_queue=queue:new(), terminal = #terminal_state{}}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(SOCKET_OPTIONS, [binary, {packet, 4}, {active, true}, {nodelay, true}, {reuseaddr, true}, {keepalive, true}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Socket) ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    ok = gen_tcp:controlling_process(Socket, Pid),
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

login(Pid, Login, Pass, Host, Port) ->
    send_request(Pid, login, [Login, Pass, Host, Port]).

test_login(Pid) ->
    login(Pid, "TCNN9956", "VYDYD8", "195.128.78.60", 3939).

logout(Pid) ->
    send_request(Pid, logout).

neworder(Pid, Market, Security, Amount) ->
    send_request(Pid, neworder, [Market, Security, Amount]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send_request(Pid, Request) ->
    send_request(Pid, Request, []).

send_request(Pid, Request, Args) ->
    send_request(Pid, Request, Args, infinity).

send_request(Pid, Request, Args, Timeout) ->
    gen_server:call(Pid, {request, Request, Args}, Timeout).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
    {ok, #state{}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({set_socket, Socket}, _, State=#state{}) ->
    Peername = trade_utils:peername(Socket),
    error_logger:info_msg("New terminal accepted: ~s~n", [Peername]),
    ok = inet:setopts(Socket, ?SOCKET_OPTIONS),
    ok = trade_terminal_manager:register_terminal(self()),
    {reply, ok, State#state{socket=Socket, endpoint=Peername}};

handle_call({request, neworder, [Market, Security, Amount]}, From, State=#state{terminal=Terminal}) ->
    try
        ClientID   = get_client_id(Terminal),
        SecurityID = get_security_id(Market, Security, Terminal),
        add_request(neworder, [SecurityID, ClientID, Amount], From, State)
    catch
        _:Error ->
            error_logger:info_msg("Error: ~p~n", [erlang:get_stacktrace()]),
            {reply, {error, Error}, State}
    end;

handle_call({request, Name, Args}, From, State) ->
    add_request(Name, Args, From, State);

handle_call(close, _, State=#state{socket=Socket}) ->
    gen_tcp:close(Socket),
    {stop, normal, ok, State};

handle_call(get_terminal_state, _, State=#state{terminal=Terminal}) ->
    {reply, Terminal, State};

handle_call(Something, _, State=#state{endpoint=Endpoint}) ->
    error_logger:info_msg("Terminal ~s receives unexpected call: ~p~n", [Endpoint, Something]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_cast(Something, State=#state{endpoint=Endpoint}) ->
    error_logger:info_msg("Terminal ~s receives unexpected cast: ~p~n", [Endpoint, Something]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_info({tcp, Socket, Data}, State=#state{socket=Socket, endpoint=_Endpoint}) ->
    error_logger:info_msg("Terminal ~s receives:~n~ts~n", [_Endpoint, Data]),
    {noreply, handle_data(parse(Data), State)};

handle_info({tcp_error, Socket, Error}, State=#state{socket=Socket, endpoint=Endpoint}) ->
    error_logger:info_msg("Terminal ~s closed: ~p~n", [Endpoint, Error]),
    {stop, {shutdown, {error, Error}}, State};

handle_info({tcp_closed, Socket}, State=#state{socket=Socket, endpoint=Endpoint}) ->
    error_logger:info_msg("Terminal ~s closed~n", [Endpoint]),
    {stop, {shutdown, tcp_closed}, State};

handle_info(Something, State=#state{endpoint=Endpoint}) ->
    error_logger:info_msg("Terminal ~s receives unexpected info: ~p~n", [Endpoint, Something]),
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(Reason, #state{request_queue=Queue}) ->
    Fun = fun(#request{from=From}) -> gen_server:reply(From, {error, Reason}) end,
    lists:foreach( Fun, queue:to_list(Queue) ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_data(Data, State=#state{endpoint=_Endpoint, terminal=Terminal, request_queue=Queue}) ->
%     error_logger:info_msg("Terminal ~s receives:~n~p~n", [_Endpoint, Data]),
    NewTerminal = update_terminal_state(Data, Terminal),
    NewState    = State#state{terminal=NewTerminal},
    case queue:peek(Queue) of
        empty -> NewState;
        {value, #request{name=Name, from=From}} ->
            case handle_response(Name, Data) of
                noreply -> NewState;
                {reply, Reply} ->
                    gen_server:reply(From, Reply),
                    NewState2 = NewState#state{request_queue=queue:drop(Queue)},
                    ok = send_request(NewState2),
                    NewState2
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_terminal_state({result,[{success,_}],[]}, State=#terminal_state{}) ->
    State; %% Ignoring result

update_terminal_state({server_status, [{id, ID}, {connected, "true"}, {recover, "true"}], _}, State=#terminal_state{}) ->
    State#terminal_state{server_status=#server_status{id=list_to_integer(ID), connected=true, recover=true}};

update_terminal_state({server_status, [{id, ID}, {connected, "true"}], _}, State=#terminal_state{}) ->
    State#terminal_state{server_status=#server_status{id=list_to_integer(ID), connected=true}};

update_terminal_state({server_status, [{id, ID}, {connected, "false"}], _}, State=#terminal_state{}) ->
    State#terminal_state{server_status=#server_status{id=list_to_integer(ID), connected=false}};

update_terminal_state({server_status, [{connected, "error"}], _}, State=#terminal_state{}) ->
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

update_terminal_state(Data, State) ->
    error_logger:info_msg("Data ignored: ~p~n", [Data]),
    State.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_response(_, {result, [{success, "false"}], [{message, [], [Error]}]}) ->
    {reply, {error, {str, Error}}};

handle_response(login, {server_status, [{id, _}, {connected, "true"}, {recover, "true"}], _}) ->
    {reply, {error, recover}};

handle_response(login, {server_status, [{id, _}, {connected, "true"}], _}) ->
    {reply, ok};

handle_response(login, {server_status, [{id, _}, {connected, "false"}], _}) ->
    {reply, {error, not_connected}};

handle_response(login, {server_status, [{connected, "error"}], [Error]}) ->
    {reply, {error, {str, Error}}};

handle_response(logout, {result,[{success,"true"}],[]}) ->
    {reply, ok};

handle_response(_Op, _Data) ->
%     error_logger:info_msg("Operation: ~p, skipped data: ~p~n", [_Op, _Data]),
    noreply.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_request(Name, Args, From, State=#state{request_queue=Queue}) ->
    Request  = #request{name=Name, args=Args, from=From},
    NewQueue = queue:in(Request, Queue),
    NewState = State#state{request_queue=NewQueue},
    case queue:is_empty(Queue) of
        true  -> ok = send_request(NewState);
        false -> ok
    end,
    {noreply, NewState}.

send_request(#state{socket=Socket, endpoint=Endpoint, request_queue=Queue}) ->
    case queue:peek(Queue) of
        empty -> ok;
        {value, #request{name=Name, args=Args}} ->
            Request = make_request(Name, Args),
            error_logger:info_msg("Terminal ~s sends:~n~ts~n", [Endpoint, iolist_to_binary(Request)]),
            gen_tcp:send(Socket, Request)
    end.

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

make_request(neworder, [SecurityID, ClientID, Amount]) ->
    Format =
    "<command id='neworder'>"
        "<secid>~B</secid>"
        "<client>~s</client>"
        "<quantity>~B</quantity>"
        "<buysell>B</buysell>"
        "<unfilled>PutInQueue</unfilled>" %% CancelBalance, ImmOrCancel
        "<bymarket/>"
        "<nosplit/>"
    "</command>",
    io_lib:format(Format, [SecurityID, ClientID, Amount]);

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
    lists:keystore(money_position, 1, Positions, Pos).

update_positions(NewPositions, Positions) ->
    lists:foldl(fun(Pos, PosList) -> update_position(Pos, PosList) end, Positions, NewPositions).

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

get_value(Name, Values) ->
    case lists:keyfind(Name, 1, Values) of
        false               -> undefined;
        {Name, Value}       -> Value;
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
