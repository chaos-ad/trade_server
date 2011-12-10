-module(trade_terminal_test).
-behaviour(trade_terminal).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include("trade_terminal_state.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([stop/1, start/1, handle_request/3, get_terminal_state/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {time=0, terminal, hist_cache=ets:new(cache, [set]), hist_pos=ets:new(pos, [set])}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

stop(_) -> ok.

start({testmode, Options}) ->
    {ok, #state{terminal=init_terminal(Options)}}.

init_terminal(Options) ->
    #terminal_state{
        positions=[
            #money_position{
                saldoin     = proplists:get_value(saldoin, Options, 0.0), %% Входящий остаток
                bought      = 0.0,
                sold        = 0.0,
                saldo       = proplists:get_value(saldo, Options, 0.0), %% Текущее сальдо
                ordbuy      = 0.0,
                ordbuycond  = 0.0
        }]
    }.

get_terminal_state(#state{terminal=Terminal}) ->
    Terminal.

set_time(Pid, Time) ->
    trade_terminal:send_request(Pid, settime, Time).

handle_request(settime, Time, State) ->
    ets:delete_all_objects(State#state.hist_pos),
    {ok, State#state{time=trade_utils:to_unixtime(Time)}};

handle_request(gethistorydata, [Symbol, Period, Bars, New], State) ->
    History =
    case ets:lookup(State#state.hist_cache, {Symbol, Period}) of
        [{{Symbol, Period}, AllHist}] -> AllHist;
        [] ->
            AllHist = lists:reverse(trade_history:get_history(Symbol, Period, {2000, 1, 1})),
            true = ets:insert(State#state.hist_cache, {{Symbol, Period}, AllHist}),
            AllHist
    end,

    Offset =
    case New of
        true  -> offsetof(History, State#state.time);
        false ->
            case ets:lookup(State#state.hist_pos, {Symbol, Period}) of
                [] -> offsetof(History, State#state.time);
                [{{Symbol, Period}, Pos}] -> Pos
            end
    end,

    Result = lists:sublist(History, Offset, Bars),
    true = ets:insert(State#state.hist_pos, {{Symbol, Period}, Offset+length(Result)}),
    {Result, State};

handle_request(neworder, [Mode, Security, Amount], State) ->
    lager:debug("Test terminal: neworder: ~p = ~p", ["Mode", Mode]),
    lager:debug("Test terminal: neworder: ~p = ~p", ["Security", Security]),
    lager:debug("Test terminal: neworder: ~p = ~p", ["Amount", Amount]),
    {ok, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

offsetof(History, Time) ->
    length(lists:takewhile(fun(Bar) -> element(1, Bar) > Time end, History)) + 1.