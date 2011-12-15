-module(trade_terminal_test).
-behaviour(trade_terminal).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include("trade_periods.hrl").
-include("trade_test_stats.hrl").
-include("trade_terminal_state.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([stop/1, start/1, handle_request/3, get_terminal_state/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {
    time,
    terminal,
    test_stats=#test_stats{},
    future=ets:new(future, [set]),
    history=ets:new(history, [set]),
    offsets=ets:new(offsets, [set]),
    updated=ets:new(updated, [set])
}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

stop(_) -> ok.

start({testmode, Options}) ->
    Money = proplists:get_value(saldo, Options, 0.0),
    Time  = proplists:get_value(time,  Options, trade_utils:local_time()),
    {ok, #state{
        time=trade_utils:to_unixtime(Time),
        terminal=#terminal_state{
            positions=[
                #money_position{
                    saldoin     = Money,
                    bought      = 0.0,
                    sold        = 0.0,
                    saldo       = Money,
                    ordbuy      = 0.0,
                    ordbuycond  = 0.0
            }]
        }
    }}.

get_terminal_state(#state{terminal=Terminal}) ->
    Terminal.

set_time(Pid, Time) ->
    trade_terminal:send_request(Pid, set_time, Time).

get_stats(Pid) ->
    trade_terminal:send_request(Pid, get_stats, []).

handle_request(get_stats, [], State=#state{test_stats=Statistics}) ->
    {Statistics#test_stats{history=lists:reverse(Statistics#test_stats.history)}, State};

handle_request(set_time, Time, State=#state{time=OldTime}) ->
    case trade_utils:to_unixtime(Time) of
        OldTime -> {ok, State};
        NewTime when NewTime < OldTime ->
            true = ets:delete_all_objects(State#state.future),
            true = ets:delete_all_objects(State#state.history),
            true = ets:delete_all_objects(State#state.updated),
            {ok, State#state{time=NewTime}};
        NewTime when NewTime > OldTime ->
            true = ets:delete_all_objects(State#state.updated),
            {ok, State#state{time=NewTime}}
    end;

handle_request(gethistorydata, [Symbol, Period, Bars, New], State) ->
    History = get_history(Symbol, Period, State),
    case New of
        true  ->
            {lists:sublist(History, Bars), State};
        false ->
            Offset = get_offset(Symbol, Period, State),
            Result = lists:sublist(History, Offset, Bars),
            true   = set_offset(Symbol, Period, Offset + Bars, State),
            {Result, State}
    end;

handle_request(neworder, [Mode, Symbol, Amount], State=#state{test_stats=Statistics}) ->
    Price =
    case get_history(Symbol, ?PERIOD_M1, State) of
        [] -> error(no_history);
        [Bar|_] -> trade_utils:close(Bar)
    end,

    NewTerminal =
    case Mode of
        buy  -> del_money(Price, State#state.terminal);
        sell -> add_money(Price, State#state.terminal)
    end,

    Entry = {Mode, Symbol, Amount, State#state.time, Price},
    NewStatHistory = [Entry|Statistics#test_stats.history],
    NewStatistics = Statistics#test_stats{history=NewStatHistory},

    {ok, State#state{terminal=NewTerminal, test_stats=NewStatistics}}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_history(Symbol, Period, #state{time=Time, future=FutPid, history=HistPid, updated=UpdPid}) ->
    Key = {Symbol, Period},
    case ets:lookup(UpdPid, Key) of
        [{Key, true}] ->
            %% History was already updated
            [{Key, History}] = ets:lookup(HistPid, Key), History;
        [] ->
            %% History needs to be updated:
            Future =
            case ets:lookup(FutPid, Key) of
                [{Key, F}] -> F;
                [] -> trade_history:get_history(Symbol, Period, {2000, 1, 1})
            end,

            History =
            case ets:lookup(HistPid, Key) of
                [{Key, H}] -> H;
                [] -> []
            end,

            {HistoryPart, NewFuture} = lists:splitwith(fun(Bar) -> trade_utils:time(Bar) < Time end, Future),
            NewHistory = lists:reverse(HistoryPart) ++ History,

            case Future =/= NewFuture of
                true  -> true = ets:insert(FutPid, {Key, NewFuture});
                false -> ok
            end,

            case NewHistory =/= History of
                true  -> true = ets:insert(HistPid, {Key, NewHistory});
                false -> ok
            end,

            true = ets:insert(UpdPid, {Key, true}),
            NewHistory
    end.

get_offset(Symbol, Period, #state{offsets=Pid}) ->
    case ets:lookup(Pid, {Symbol, Period}) of
        [] -> 1;
        [{{Symbol, Period}, Offset}] -> Offset
    end.

set_offset(Symbol, Period, Offset, #state{offsets=Pid}) ->
    ets:insert(Pid, {{Symbol, Period}, Offset}).

add_money(Money, State=#terminal_state{positions=Positions}) ->
    MoneyPos = lists:keyfind(money_position, 1, Positions),
    NewMoneyPos = MoneyPos#money_position{saldo=MoneyPos#money_position.saldo+Money},
    NewPositions = lists:keyreplace(money_position, 1, Positions, NewMoneyPos),
    State#terminal_state{positions=NewPositions}.

del_money(Money, State=#terminal_state{positions=Positions}) ->
    MoneyPos = lists:keyfind(money_position, 1, Positions),
    case MoneyPos#money_position.saldo < Money of
        true  -> exit({money_underflow, MoneyPos#money_position.saldo, Money});
        false ->
            NewMoneyPos = MoneyPos#money_position{saldo=MoneyPos#money_position.saldo-Money},
            NewPositions = lists:keyreplace(money_position, 1, Positions, NewMoneyPos),
            State#terminal_state{positions=NewPositions}
    end.

