-module(trade_strategy_tester).
-compile(export_all).

-record(state, {cost=0, total=0, strategy, strategy_state}).

start(Symbol, Period, From, To, Strategy, StrategyArgs) ->
    Bars = trade_history:get_history(Symbol, Period, From, To),
    case erlang:apply(Strategy, init, StrategyArgs) of
        {ok, State} ->
            run(Bars, [], #state{strategy=Strategy, strategy_state=State})
    end.

run([], _, State) -> State;
run([Bar|Tail], History, State=#state{strategy=Strategy, strategy_state=StrategyState}) ->
    NewHistory = [Bar|History],
    NewState   =
    case Strategy:update(NewHistory, StrategyState) of
        {buy,  NewStrategyState} -> buy(Bar, set_state(NewStrategyState, State));
        {sell, NewStrategyState} -> sell(Bar, set_state(NewStrategyState, State));
        {_,    NewStrategyState} -> set_state(NewStrategyState, State)
    end,
    run(Tail, NewHistory, NewState).

buy(Bar, State=#state{cost=Cost}) ->
    case Cost =:= 0 of
        true  -> State#state{cost=trade_utils:close(Bar)};
        false -> State
    end.

sell(Bar, State=#state{cost=Cost, total=Total}) ->
    case Cost =/= 0 of
        true  -> State#state{cost=0, total=Total+(trade_utils:close(Bar)-Cost)};
        false -> State
    end.

set_state(StrategyState, State=#state{}) -> State#state{strategy_state=StrategyState}.
%
% invoke({M, F}, Args)    -> erlang:apply(M, F, Args);
% invoke({M, F, A}, Args) -> erlang:apply(M, F, Args ++ A);
% invoke(Functor, Args)   -> erlang:apply(Functor, Args).
