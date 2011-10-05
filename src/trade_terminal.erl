-module(trade_terminal).
-compile(export_all).

% -include("trade_terminal.hrl").
-include_lib("xmerl/include/xmerl.hrl").

% -export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {socket, endpoint, client_id, securities}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(SOCKET_OPTIONS, [binary, {packet, 4}, {active, false}, {nodelay, true}, {reuseaddr, true}, {keepalive, true}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Socket) ->
    {ok, Pid} = gen_server:start(?MODULE, Socket, []),
    ok = gen_tcp:controlling_process(Socket, Pid),
    {ok, Pid}.

login(Pid, Login, Pass, Host, Port) ->
    gen_server:call(Pid, {login, Login, Pass, Host, Port}, infinity).

logout(Pid) ->
    gen_server:call(Pid, logout).

get_client_id(Pid) ->
    gen_server:call(Pid, get_client_id).

get_securities(Pid) ->
    gen_server:call(Pid, get_securities).

read_all(Pid) ->
    gen_server:call(Pid, read_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Socket) ->
    ok = inet:setopts(Socket, ?SOCKET_OPTIONS),
    Peername = trade_utils:peername(Socket),
    error_logger:info_msg("New terminal accepted: ~s~n", [Peername]),
    ok = trade_terminal_manager:register_terminal(self()),
    timer:send_interval(5000, ping),
    {ok, #state{socket=Socket, endpoint=Peername}}.

handle_call(logout, _, State=#state{socket=Socket}) ->
    ok = do_send(Socket, "<command id='disconnect'/>"),
    Reply = parse(recv_until(<<"^<result success">>, State)),
    {reply, Reply, State};

handle_call({login, Login, Pass, Host, Port}, _, State=#state{socket=Socket}) ->
    Format =
    "<command id='connect'>"
        "<login>~s</login><password>~s</password><host>~s</host><port>~B</port>"
        "<logsdir>./logs/</logsdir><loglevel>0</loglevel>"
    "</command>",
    ok = do_send(Socket, io_lib:format(Format, [Login, Pass, Host, Port])),

    Response = parse(recv_until(Socket, <<"^<server_status|^<result success=\"false\">">>)),

    case lists:keyfind(result, 1, Response) of
        {result, [{success, "true" }], _} -> ok;
        {result, [{success, "false"}], _} -> exit(invalid_request);
        false                             -> exit(invalid_response)
    end,

    case lists:keyfind(server_status, 1, Response) of
        {server_status, [{id,_}, {connected,"true" }], []}       -> ok;
        {server_status, [        {connected,"error"}], [Reason]} -> exit(Reason);
        Other1                                                   -> exit({invalid_response, Other1})
    end,

    Securities =
    case lists:keyfind(securities, 1, Response) of
        {securities, [], List} -> lists:map(fun simplify_security/1, List);
        Other2                 -> exit({invalid_response, Other2})
    end,

    ClientID =
    case lists:keyfind(client, 1, Response) of
        {client, [{id,ID},{remove,_}], _} -> ID;
        Other3                            -> exit({invalid_response, Other3})
    end,

    {reply, ok, State#state{client_id=ClientID, securities=Securities}};

handle_call(get_client_id, _, State=#state{client_id=ClientID}) ->
    {reply, ClientID, State};

handle_call(get_securities, _, State=#state{securities=Securities}) ->
    {reply, Securities, State};

handle_call(_, _, State) -> {reply, {error, invalid_request}, State}.
handle_cast(_, State) -> {noreply, State}.

handle_info(ping, State=#state{socket=Socket}) ->
    case do_send(Socket, <<"PING">>) of
        {error, Error} -> {stop, {shutdown, Error}, State};
        ok ->
            case do_recv(Socket, 1000) of
                {error, Error}   -> {stop, {shutdown, Error}, State};
                {ok, <<"PONG">>} -> {noreply, State}
            end
    end;

handle_info(_, State) -> {noreply, State}.
code_change(_, State, _) -> {ok, State}.
terminate(_, _) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% send(Socket, Data) ->
%     case gen_tcp:send(Socket, Data) of
%         ok             -> {reply, ok, State};
%         {error, Error} -> {stop, normal, {error, Error}, State}
%     end;

do_send(Socket, Data) ->
    gen_tcp:send(Socket, Data).

do_recv(Socket) -> do_recv(Socket, 0).
do_recv(Socket, Time) when is_integer(Time), Time > 0 ->
    gen_tcp:recv(Socket, 0, Time);
do_recv(Socket, _) ->
    gen_tcp:recv(Socket, 0).

recv_all(Socket) -> recv_all(Socket, 100).
recv_all(Socket, Timeout) -> lists:reverse( recv_all(Socket, Timeout, []) ).
recv_all(Socket, Timeout, Result) ->
    case do_recv(Socket, Timeout) of
        {error, _} -> Result;
        {ok, Data} -> recv_all(Socket, Timeout, [Data|Result])
    end.

recv_until(Socket, Pattern) ->
    lists:reverse( recv_until(Socket, Pattern, []) ).

recv_until(Socket, Pattern, Result) ->
    {ok, Response} = do_recv(Socket),
    NewResult = [Response | Result],
    case re:run(Response, Pattern) of
        nomatch -> recv_until(Socket, Pattern, NewResult);
        _       -> NewResult
    end.

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

parse(Data=[Bin|_]) when is_binary(Bin) ->
    lists:map(fun(X) -> parse(X) end, Data);

parse(Bin) when is_binary(Bin) ->
    case xmerl_scan:string(binary_to_list(Bin)) of
        {Result, []} -> xmerl_lib:simplify_element(Result);
        _            -> exit(parse_error)
    end.


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
simplify_security_prop({opmask, Attributes, []})    -> Attributes;
simplify_security_prop({Name, [], [Value]})         -> {Name, Value}.

simplify_security({security, Attributes, Properties}) ->
    {hd(element(3, lists:keyfind(seccode,   1, Properties))),
     list_to_integer(element(2, lists:keyfind(secid,     1, Attributes))),
     hd(element(3, lists:keyfind(shortname, 1, Properties)))}.
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
