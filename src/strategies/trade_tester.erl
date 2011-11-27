-module(trade_tester).
-compile(export_all).

test(Symbol, Period, From, Module, Options) ->
    Data = trade_history:get_history(Symbol, Period, From),
    State = Module:start(Options),
    test(Module, Data, [], State).

test(Symbol, Period, From, To, Module, Options) ->
    Data = trade_history:get_history(Symbol, Period, From, To),
    State = Module:start(Options),
    test(Module, Data, [], State).

test(_, [], _, State) -> State;
test(Module, [Bar|Tail], History, State) ->
    NewHist = [Bar|History],
    case Module:update(NewHist, State) of
        {hold, NewState} -> ok;
        {buy,  NewState} -> lager:info("Strategy ~p: buy  signal on bar ~p", [Module, beautify(Bar)]);
        {sell, NewState} -> lager:info("Strategy ~p: sell signal on bar ~p", [Module, beautify(Bar)])
    end,
    test(Module, Tail, NewHist, NewState).

beautify(Bar) ->
    Time = element(1, Bar),
    setelement(1, Bar, trade_utils:to_datetimestr(Time)).
