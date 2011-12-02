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

-export([behaviour_info/1]).

behaviour_info(callbacks) -> [
    {stop, 1},
    {start, 1},
    {handle_request, 3},
    {get_terminal_state, 1}         %% should return #terminal_state{}
];
behaviour_info(_) -> undefined.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {account, module, data}).

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
    send_request(Pid, neworder, [Mode, SecurityID, ClientID, Amount]).

cancel_order(Pid, TransactionID) ->
    send_request(Pid, cancelorder, [TransactionID]).

get_history(Pid, Market, Security, Period, Bars) ->
    get_history(Pid, Market, Security, Period, Bars, true).

get_history(Pid, Market, Security, Period, Bars, New) ->
    get_history(Pid, Market, Security, Period, Bars, New, 10000).

get_history(Pid, Market, Security, Period, Bars, New, Timeout) ->
    Terminal    = get_state(Pid),
    PeriodID    = get_period_id(Period, Terminal),
    SecurityID  = get_security_id(Market, Security, Terminal),
    send_request(Pid, gethistorydata, [SecurityID, PeriodID, Bars, New], Timeout).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send_request(Pid, Request) ->
    send_request(Pid, Request, []).

send_request(Pid, Request, Args) ->
    send_request(Pid, Request, Args, infinity).

send_request(Pid, Request, Args, Timeout) ->
    gen_server:call(Pid, {request, Request, Args}, Timeout).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init({testmode, Options}) ->
    lager:debug("Starting test terminal..."),
    {ok, Data} = trade_terminal_test:start({testmode, Options}),
    lager:debug("Test terminal started: ~p", [self()]),
    {ok, #state{account=testmode, module=trade_terminal_test, data=Data}};

init({Account, Options}) ->
    lager:info("Starting terminal '~p'...", [Account]),
    {ok, Data} = trade_terminal_impl:start({Account, Options}),
    lager:info("Terminal '~p' started: ~p", [Account, self()]),
    {ok, #state{account=Account, module=trade_terminal_impl, data=Data}}.

handle_call({stop, Reason}, _, State) ->
    {stop, Reason, ok, State};

handle_call(get_terminal_state, _, State=#state{module=Module, data=Data}) ->
    {reply, Module:get_terminal_state(Data), State};

handle_call({request, Request, Args}, _, State=#state{module=Module, data=Data}) ->
    lager:debug("Handling request ~p...", [Request]),
    case Module:handle_request(Request, Args, Data) of
        {error, Error} ->
            lager:error("Failed to perform request ~p: ~p", [Request, Error]),
            {reply, {error, Error}, State};
        {Reply, NewData} ->
            lager:debug("Request ~p performed successfully", [Request]),
            {reply, Reply, State#state{data=NewData}}
    end;

handle_call(Msg, From, State=#state{module=Module, data=Data}) ->
    case has_function(Module, handle_call, 3) of
        true  ->
            case Module:handle_call(Msg, From, Data) of
                {noreply,      NewData} -> {noreply,      State#state{data=NewData}};
                {reply, Reply, NewData} -> {reply, Reply, State#state{data=NewData}};
                {stop, Reason, NewData} -> {stop, Reason, State#state{data=NewData}}
            end;
        false ->
            {reply, {error, invalid_request}, State}
    end.

handle_cast(Msg, State=#state{module=Module, data=Data}) ->
    case has_function(Module, handle_cast, 2) of
        true  ->
            case Module:handle_cast(Msg, Data) of
                {noreply, NewData} -> {noreply, State#state{data=NewData}};
                {stop, Reason, NewData} -> {stop, Reason, State#state{data=NewData}}
            end;
        false ->
            {noreply, State}
    end.

handle_info(Msg, State=#state{module=Module, data=Data}) ->
    case has_function(Module, handle_info, 2) of
        true  ->
            case Module:handle_info(Msg, Data) of
                {noreply, NewData} -> {noreply, State#state{data=NewData}};
                {stop, Reason, NewData} -> {stop, Reason, State#state{data=NewData}}
            end;
        false ->
            {noreply, State}
    end.

terminate(Reason, #state{account=testmode, module=Module, data=Data}) ->
    Module:stop(Data),
    lager:debug("Test terminal stopped: ~p", [Reason]);

terminate(Reason, #state{account=Account, module=Module, data=Data}) ->
    Module:stop(Data),
    lager:info("Terminal '~p' stopped: ~p", [Account, Reason]).

code_change(_, State, _) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

has_function(Module, Function, Arity) ->
    erlang:function_exported(Module, Function, Arity).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_client_id(#terminal_state{clients=[#client{id=ID}]}) -> ID;
get_client_id(#terminal_state{}) -> exit(invalid_client).

get_period_id(Period, #terminal_state{candlekinds=Candles}) ->
    case lists:keyfind(Period*60, 3, Candles) of
        #candlekind{id=ID} -> ID;
        false          -> exit(invalid_period)
    end.

get_security_id(Market, Security, #terminal_state{securities=Securities}) ->
    get_security_id(Market, Security, Securities);

get_security_id(Market, Security, Securities) when is_list(Securities) ->
    case lists:keytake(Security, 5, Securities) of
        {value, #security{secid=ID, market=Market}, _} -> ID;
        {value, #security{}, Rest} -> get_security_id(Market, Security, Rest);
        false -> exit(invalid_security)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
