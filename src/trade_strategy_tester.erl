-module(trade_strategy_tester).
% -compile(export_all).
%
% -record(state, {
%     mod,
%     data,
%     money = 0,
%     lots  = 0,
%     total_bids = 0,
%     total_signals = 0
% }).
%
% test(Symbol, Period, Money, TimeFrom, Strategy, StrategyOptions) ->
%     History = trade_history:get_history(Symbol, Period, TimeFrom),
%     State = #state{money=Money, mod=Strategy, data=Strategy:start(StrategyOptions)},
%     test(History, [], State).
%
% test(Symbol, Period, Money, TimeFrom, TimeTo, Strategy, StrategyOptions) ->
%     History = trade_history:get_history(Symbol, Period, TimeFrom, TimeTo),
%     State = #state{money=Money, mod=Strategy, data=Strategy:start(StrategyOptions)},
%     test(History, [], State).
%
% test([], _, State) ->
%     print_report(State), State;
%
% test([Bar|Tail], History, State=#state{mod=Strategy, data=StrategyState}) ->
%     NewHistory = [Bar|History],
%     {Signal, NewStrategyState} = Strategy:update(NewHistory, StrategyState),
%     NewState = State#state{data=NewStrategyState},
%     test(Tail, NewHistory, handle_signal(clamp_signal(Signal), Bar, NewState)).
%
% clamp_signal(Signal) -> round(Signal).
%
% handle_signal(N, Bar, State=#state{lots=M}) when N > M -> buy(M-N, Bar, State);
% handle_signal(N, Bar, State=#state{lots=M}) when N < M -> sell(M-N, Bar, State);
% handle_signal(N,   _, State=#state{lots=N}) -> State.
%
% handle_signal(_, State) -> State.
%
% buy(N, Bar, State=#state{lots=M, money=Money}) ->
%     Price = trade_utils:close(Bar) * N,
%     case Price > Money of
%         true  ->
%             lager:debug("No money"),
%             State1 = State;
%         false ->
%             lager:debug("Buy ~B with price ~p", [N, Price]),
%             State0 = State#state{money=Money-Price, lots=M+N},
%             State1 = upd_stats(total_bids, State0)
%     end,
%     upd_stats(total_signals, State1).
%
% sell(N, Bar, State=#state{lots=M, money=Money}) when N =< M ->
%     Price = trade_utils:close(Bar) * N,
%     lager:debug("Sell ~B with price ~p", [N, Price]),
%     State0 = State#state{money=Money+Price, lots=M-N},
%     State1 = upd_stats(total_bids, State0),
%     upd_stats(total_signals, State1).
%
% upd_stats(_, State) -> State.
%
% print_report(_State) -> ok.
%
% beautify(Bar) ->
%     Time = element(1, Bar),
%     setelement(1, Bar, trade_utils:to_datetimestr(Time)).
