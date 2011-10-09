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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

login(Pid, Login, Pass, Host, Port) ->
    send_request(Pid, login, [Login, Pass, Host, Port]).

test_login(Pid) ->
    login(Pid, "TCNN9956", "VYDYD8", "195.128.78.60", 3939).

logout(Pid) ->
    send_request(Pid, logout).

send_test_order(Pid, SecCode, Quantity) ->
    send_request(Pid, neworder, [SecCode, Quantity]).

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

handle_info({tcp, Socket, Data}, State=#state{socket=Socket, endpoint=Endpoint}) ->
    error_logger:info_msg("Terminal ~s receives:~n~ts~n", [Endpoint, Data]),
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

handle_data(Data, State=#state{terminal=Terminal, request_queue=Queue}) ->
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

update_terminal_state({server_status, [{id, ID}, {connected, "true"}, {recover, "true"}], _}, State=#terminal_state{}) ->
    State#terminal_state{server_status=#server_status{id=list_to_integer(ID), connected=true, recover=true}};

update_terminal_state({server_status, [{id, ID}, {connected, "true"}], _}, State=#terminal_state{}) ->
    State#terminal_state{server_status=#server_status{id=list_to_integer(ID), connected=true}};

update_terminal_state({server_status, [{id, ID}, {connected, "false"}], _}, State=#terminal_state{}) ->
    State#terminal_state{server_status=#server_status{id=list_to_integer(ID), connected=false}};

update_terminal_state({server_status, [{connected, "error"}], _}, State=#terminal_state{}) ->
    State#terminal_state{server_status=#server_status{connected=false}};

update_terminal_state(_, State) -> State.

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

handle_response(Op, Data) ->
    error_logger:info_msg("Operation: ~p, skipped data: ~p~n", [Op, Data]),
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

make_request(neworder, Args) ->
    Format= "<command id='neworder'>"
                "<secid>~B</secid><client>~s</client><quantity>~B</quantity>"
                "<buysell>B</buysell><bymarket/><brokerref>no</brokerref>"
                "<unfilled>ImmOrCancel</unfilled><usecredit/><nosplit/>"
            "</command>",
    io_lib:format(Format, Args);

make_request(login, Args) ->
    Format= "<command id='connect'>"
                "<login>~s</login><password>~s</password><host>~s</host><port>~B</port>"
                "<logsdir>./logs/</logsdir><loglevel>0</loglevel>"
                "<rqdelay>500</rqdelay><session_timeout>25</session_timeout><request_timeout>20</request_timeout>"
            "</command>",
    io_lib:format(Format, Args);

make_request(disconnect,    []) -> "<command id='disconnect'/>";
make_request(server_status, []) -> "<command id='server_status'/>".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse(Bin) when is_binary(Bin) ->
    case xmerl_scan:string(binary_to_list(Bin)) of
        {Result, []} -> xmerl_lib:simplify_element(Result);
        _            -> exit(parse_error)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% update_terminal_state(Status=#server_status{}, State=#state{terminal=Terminal, handlers=Handlers}) ->
%     {ok, State#state{terminal=Terminal#terminal_state{server_status=Status}}};

% invoke_handlers(#state{





%
% handle_data({result,[{success,"false"}],[{message,[],[Error]}]}, connecting, State=#state{from=From})
%         when From =/= undefined ->
%     gen_fsm:reply(From, {error, {str, Error}}),
%     {next_state, disconnected, State#state{from=undefined}};
%
% handle_data({server_status, Attributes, Fields}, connecting, State=#state{from=From}) ->
%     case proplists:get_value(connected, Attributes) of
%         "true" ->
%             gen_fsm:reply(From, ok),
%             {next_state, connected, State#state{from=undefined}};
%         "error" ->
%             [Error] = Fields,
%             gen_fsm:reply(From, {error, {str, Error}}),
%             {next_state, disconnected, State#state{from=undefined}}
%     end;
%
% handle_data({candlekinds,[], CandleTypes}, _, State=#state{tstate=TState}) ->
%     Fun = fun({kind,[],[{id,[],[ID]},{period,[],[Period]},{name,[],[Name]}]}) ->
%             {list_to_integer(ID), list_to_integer(Period), Name}
%           end,
%     {ok, State#state{tstate=TState#terminal_state{candles=lists:map(Fun, CandleTypes)}}};


% request(Type, From, State) ->
%     request(Type, [], From, State).
% 
% request(Type, Args, From, State=#state{socket=Socket, endpoint=Endpoint, request_queue=Queue}) ->
%     Request = make_request(Type, Args),
%     error_logger:info_msg("Sending data to terminal ~s:~n~ts~n", [Endpoint, Request]),
%     case gen_tcp:send(Socket, Request) of
%         ok             -> {noreply, State#state{request_queue=queue:in({Type, From}, Queue)}};
%         {error, Error} -> {stop, normal, {error, Error}, State}
%     end.

% send(Socket, Data) ->
%     error_logger:info_msg("Sent:~n~p~n", [Data]),
%     gen_tcp:send(Socket, Data).
% 
% recv(Socket) -> recv(Socket, 0).
% recv(Socket, Time) when is_integer(Time), Time > 0 ->
%     R = gen_tcp:recv(Socket, 0, Time),
%     error_logger:info_msg("Received:~n~p~n", [R]),
%     R;
% 
% recv(Socket, _) ->
%     R = gen_tcp:recv(Socket, 0),
%     error_logger:info_msg("Received:~n~p~n", [R]),
%     R.
% 
% recv_all(Socket) -> recv_all(Socket, 100).
% recv_all(Socket, Timeout) -> lists:reverse( recv_all(Socket, Timeout, []) ).
% recv_all(Socket, Timeout, Result) ->
%     case recv(Socket, Timeout) of
%         {error, _} -> Result;
%         {ok, Data} -> recv_all(Socket, Timeout, [Data|Result])
%     end.
% 
% recv_until(Socket, Pattern) ->
%     lists:reverse( recv_until(Socket, Pattern, []) ).
% 
% recv_until(Socket, Pattern, Result) ->
%     {ok, Response} = recv(Socket),
%     NewResult = [Response | Result],
%     case re:run(Response, Pattern) of
%         nomatch -> recv_until(Socket, Pattern, NewResult);
%         _       -> NewResult
%     end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% login(Socket, Login, Pass, Host, Port) ->
%     Format =
%     "<command id='connect'>"
%         "<login>~s</login>"
%         "<password>~s</password>"
%         "<host>~s</host>"
%         "<port>~B</port>"
%         "<logsdir>./logs/</logsdir>"
%         "<loglevel>0</loglevel>"
%     "</command>",
%     ok = send(Socket, iolist_to_binary(io_lib:format(Format, [Login, Pass, Host, Port]))),
%     recv_until(<<"^<server_status|^<result success=\"false\">">>, Pid).
% 
% test_login(Pid) ->
%     login(Pid, "TCNN9956", "VYDYD8", "195.128.78.60", 3939).
% 
% logout(Pid) ->
%     ok = send(Pid, "<command id='disconnect'/>"),
%     recv_until(<<"^<result success">>, Pid).
% 
% server_status(Pid) ->
%     ok = send(Pid, "<command id='server_status'/>"),
%     recv_until(<<"^<server_status|^<result success=\"false\">">>, Pid).
% 
% get_securities(Pid) ->
%     ok = send(Pid, "<command id='get_securities'/>"),
%     parse(securities, recv_until(<<"</securities>$|^<result success=\"false\">">>, Pid)).
% 
% get_hist_data(Pid, ID, Period, Count) when is_integer(ID), is_integer(Period), is_integer(Count) ->
%     Msg = "<command id='gethistorydata' secid='~B' period='~B' count='~B' reset='true'/>",
%     send(Pid, io_lib:format(Msg, [ID, Period, Count])).
% 
% send_order(Pid) ->
%     Cmd =
%     "<command id='neworder'>"
%         "<secid>6374</secid>"
%         "<client>virt/9956</client>"
%         "<quantity>1</quantity>"
%         "<buysell>B</buysell>"                  %% "('В' - покупка, или 'S' – продажа)
%         "<bymarket/>"
%         "<brokerref>примечание</brokerref>" %% (будет возвращено в составе структур order и trade)
%         "<unfilled>ImmOrCancel</unfilled>" %% (другие возможные значения: CancelBalance, ImmOrCancel)
%         "<usecredit/>"
%         "<nosplit/>"
%     "</command>",
%     send(Pid, Cmd).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% parse(Data=[Bin|_]) when is_binary(Bin) ->
%     lists:map(fun(X) -> parse(X) end, Data);


% simplify_security({security, Attributes, Values}) ->
%     lists:map(fun({Tag, Attr, []} -> Attr; ({Tag, [], Val}) -> 
%     {security, 
%     {security, [get_value(secid,     Attributes),
%                 get_value(decimals,  Values),
%                 get_value(seccode,   Values),
%                 get_value(shortname, Values),
%                 get_value(minstep,   Values),
%                 get_value(lotsize,   Values)]}.

% simplify_security_prop({market, [], [Value]})       -> {name, list_to_float(Value)};
% simplify_security_prop({lotsize, [], [Value]})      -> {name, list_to_float(Value)};
% simplify_security_prop({minstep, [], [Value]})      -> {name, list_to_float(Value)};
% simplify_security_prop({decimals, [], [Value]})     -> {name, list_to_float(Value)};
% simplify_security_prop({point_cost, [], [Value]})   -> {name, list_to_float(Value)};
% simplify_security_prop({opmask, Attributes, []})    -> Attributes;
% simplify_security_prop({Name, [], [Value]})         -> {Name, Value}.
% 
% simplify_security({security, Attributes, Properties}) ->
%     {hd(element(3, lists:keyfind(seccode,   1, Properties))),
%      list_to_integer(element(2, lists:keyfind(secid,     1, Attributes))),
%      hd(element(3, lists:keyfind(shortname, 1, Properties)))}.
%     L1 = lists:keysort(1, Attributes),
%     L2 = lists:keysort(1, lists:flatten(lists:map(fun simplify_security_prop/1, Properties))),
%     {security, lists:keymerge(1, L1, L2)}.

% 
% simplify_security({security, [{secid,"834"},{active,"true"}],
%                              [{seccode,[],["GRAZ"]},
%                               {shortname,[],
%                                [[1056,1072,1079,1075,1091,1083,1103,1081]]},
%                               {decimals,[],["2"]},
%                               {market,[],["1"]},
%                               {sectype,[],["SHARE"]},
%                               {opmask,
%                                [{usecredit,"yes"},
%                                 {bymarket,"no"},
%                                 {nosplit,"yes"},
%                                 {immorcancel,"yes"},
%                                 {cancelbalance,"yes"}],
%                                []},
%                               {minstep,[],["0.01"]},
%                               {lotsize,[],["100"]},
%                               {point_cost,[],["1"]}]},

% get_value(Name, Values) ->
%     case lists:keyfind(Name, 1, Values) of
%         {Name, Value}       -> {Name, Value};
%         {Name, [], [Value]} -> {Name, Value}
%     end.
