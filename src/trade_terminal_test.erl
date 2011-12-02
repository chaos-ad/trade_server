-module(trade_terminal_test).
-behaviour(trade_terminal).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include("trade_terminal_state.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([stop/1, start/1, handle_request/3, get_terminal_state/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

stop(_) -> ok.

start({testmode, Options}) ->
    {ok, #terminal_state{
        positions=[
            #money_position{
                saldoin     = proplists:get_value(saldoin, Options, 0.0), %% Входящий остаток
                bought      = 0.0,
                sold        = 0.0,
                saldo       = proplists:get_value(saldo, Options, 0.0), %% Текущее сальдо
                ordbuy      = 0.0,
                ordbuycond  = 0.0
        }]
    }}.

get_terminal_state(State) ->
    State.

handle_request(neworder, [Mode, SecurityID, ClientID, Amount], State=#terminal_state{}) ->
    lager:debug("Test terminal: neworder: ~p = ~p", ["Mode", Mode]),
    lager:debug("Test terminal: neworder: ~p = ~p", ["Mode", SecurityID]),
    lager:debug("Test terminal: neworder: ~p = ~p", ["Mode", ClientID]),
    lager:debug("Test terminal: neworder: ~p = ~p", ["Mode", Amount]),
    {ok, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
