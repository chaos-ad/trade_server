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

-record(state, {time=0, terminal, test_stats=#test_stats{}, hist_cache=ets:new(cache, [set]), hist_pos=ets:new(pos, [set])}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

stop(_) -> ok.

start({testmode, Options}) ->
    {ok, #state{terminal=init_terminal(Options)}}.

init_terminal(Options) ->
    Money = proplists:get_value(saldo, Options, 0.0),
    #terminal_state{
        positions=[
            #money_position{
                saldoin     = Money,
                bought      = 0.0,
                sold        = 0.0,
                saldo       = Money,
                ordbuy      = 0.0,
                ordbuycond  = 0.0
        }]
    }.

get_terminal_state(#state{terminal=Terminal}) ->
    Terminal.

set_time(Pid, Time) ->
    trade_terminal:send_request(Pid, settime, Time).

get_stats(Pid) ->
    trade_terminal:send_request(Pid, get_stats, []).

handle_request(get_stats, [], State=#state{test_stats=Statistics}) ->
    {Statistics#test_stats{history=lists:reverse(Statistics#test_stats.history)}, State};

handle_request(settime, Time, State) ->
    case Time >= State#state.time of
        true  -> ok;
        false -> ets:delete_all_objects(State#state.hist_pos)
    end,
    {ok, State#state{time=trade_utils:to_unixtime(Time)}};

handle_request(gethistorydata, [Symbol, Period, Bars, New], State) ->

    History = get_history(State#state.hist_cache, Symbol, Period),

    Offset =
    case New of
        true  -> offsetof(History, State#state.time);
        false ->
            case ets:lookup(State#state.hist_pos, {Symbol, Period}) of
                [{{Symbol, Period}, Pos}] -> Pos;
                [] -> offsetof(History, State#state.time)
            end
    end,

    Result = lists:sublist(History, Offset, Bars),
    true = ets:insert(State#state.hist_pos, {{Symbol, Period}, Offset+length(Result)}),
    {Result, State};

handle_request(neworder, [Mode, Symbol, Amount], State=#state{test_stats=Statistics}) ->

    History = get_history(State#state.hist_cache, Symbol, ?PERIOD_M1),
    Bar = first_bar(History, State#state.time),
    Price = trade_utils:close(Bar),

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

get_history(CachePid, Symbol, Period) ->
    case ets:lookup(CachePid, {Symbol, Period}) of
        [{{Symbol, Period}, AllHist}] -> AllHist;
        [] ->
            AllHist = lists:reverse(trade_history:get_history(Symbol, Period, {2000, 1, 1})),
            true = ets:insert(CachePid, {{Symbol, Period}, AllHist}),
            AllHist
    end.

offsetof(History, Time) ->
    length(lists:takewhile(fun(Bar) -> element(1, Bar) > Time end, History)) + 1.

first_bar([], _) -> undefined;
first_bar([Bar| Tail], Time) when element(1, Bar)  > Time -> first_bar(Tail, Time);
first_bar([Bar|_Tail], Time) when element(1, Bar) =< Time -> Bar.

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

