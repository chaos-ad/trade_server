-module(trade_tester).
-compile(export_all).

-include_lib("trade_periods.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(stats, {equity=0, pl=[], bids=[], ops=[]}).
-record(state, {lots=0, price=0, money=0, s_module, s_state, stats=#stats{}, history=[], future=[]}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(Symbol, Period, From, Strategy, Options, TestOptions) ->
    State = #state{
        lots     = 0,
        money    = proplists:get_value(money, TestOptions),
        history  = [],
        future   = trade_history:get_history(Symbol, Period, From),
        s_module = Strategy,
        s_state  = Strategy:start(Options)
    },
    test_loop(State).


test_loop(State=#state{s_module=SModule, future=[], stats=Stats}) ->
    SModule:stop(State#state.s_state),
    PL   = lists:reverse(Stats#stats.pl),
    Ops  = lists:reverse(Stats#stats.ops),
    Bids = lists:reverse(Stats#stats.bids),
    NewStats = Stats#stats{pl=PL, ops=Ops, bids=Bids},
%     print_report(NewStats),
    NewStats;

test_loop(State=#state{s_module=SModule, history=History, future=[Bar|Future]}) ->
    NewState1 = State#state{history=[Bar|History], future=Future},
    NewState2 = update_state( SModule:update(NewState1#state.history, NewState1#state.s_state), NewState1 ),
    test_loop(NewState2).


update_state({Signal, NewSState}, State) ->
    handle_signal(Signal, State#state{s_state=NewSState}).


%% Покупка:
handle_signal(N, State=#state{lots=L, price=AvgPrice, money=Money, stats=Stats}) when N > L, N > 0 ->
    Lots = N - L,
    Bar = hd(State#state.history),
    BuyPrice = trade_utils:close(Bar),
    NewAvgPrice = (AvgPrice * L + BuyPrice * Lots) / N,

%     lager:debug("tester: ~s: buy  ~p at ~p", [trade_utils:to_datetimestr(trade_utils:time(Bar)), Lots, BuyPrice]),

    case (BuyPrice * Lots) > Money of
        true  -> exit(no_money);
        false -> ok
    end,

    OP  = {trade_utils:time(Bar), -Lots, BuyPrice},
    NewStats = Stats#stats{
        ops    = [OP |Stats#stats.ops],
        equity = Money-(BuyPrice * Lots)
    },

    State#state{lots=N, price=NewAvgPrice, money=Money-(BuyPrice * Lots), stats=NewStats};


%% Продажа:
handle_signal(N, State=#state{lots=L, price=AvgPrice, money=Money, stats=Stats}) when L > N, L > 0 ->
    Lots = L - N,
    Bar = hd(State#state.history),
    SellPrice = trade_utils:close(Bar),

%     lager:debug("tester: ~s: sell ~p at ~p", [trade_utils:to_datetimestr(trade_utils:time(Bar)), Lots, SellPrice]),

    PL  = (SellPrice * Lots) / (AvgPrice * Lots),
    OP  = {trade_utils:time(Bar), -Lots, SellPrice},
    BID = {trade_utils:time(Bar), AvgPrice*Lots, SellPrice*Lots},
    NewStats = Stats#stats{
        pl     = [PL |Stats#stats.pl],
        bids   = [BID|Stats#stats.bids],
        ops    = [OP |Stats#stats.ops],
        equity = Money+(SellPrice * Lots)
    },

    State#state{lots=L-Lots, money=Money+(SellPrice * Lots), stats=NewStats};

handle_signal(_, State) ->
    State.

print_report(#stats{pl=PL, bids=Bids, equity=Money}) ->
    lager:info("Final equity: ~p~n", [Money]),
    lager:info("Geometric mean: ~p~n", [trade_utils:geometric_mean(PL)]),
    lager:info("Total bids: ~p~n", [length(PL)]),
    lager:info("Total win bids: ~p~n", [length(lists:filter(fun(X) -> X > 1 end, PL))]),
    lager:info("Total loose bids: ~p~n", [length(lists:filter(fun(X) -> X =< 1 end, PL))]),
    lager:info("Maximal win: ~p~n", [lists:max(lists:map(fun({_,X,Y}) -> Y - X end, Bids))]),
    lager:info("Maximal loose: ~p~n", [lists:min(lists:map(fun({_,X,Y}) -> Y - X end, Bids))]).
