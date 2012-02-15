-module(bar_strategy).
% -behaviour(trade_strategy).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start/3, stop/1, get_stats/1, handle_cast/2]).

-export([behaviour_info/1]).

behaviour_info(callbacks) -> [{start, 2}, {update, 3}, {stop, 2}];
behaviour_info(_) -> undefined.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {name, terminal, strategy, strategy_state, security, period, depth, last_bar}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Name, Terminal, Options) ->
    {Strategy, StrategyOptions} = proplists:get_value(strategy, Options),
    TerminalState = trade_terminal:get_state(Terminal),
    State = #state{
        name            = Name,
        terminal        = Terminal,
        strategy        = Strategy,
        strategy_state  = Strategy:start(TerminalState, StrategyOptions),
        security        = proplists:get_value(security, Options),
        period          = proplists:get_value(period, Options),
        depth           = proplists:get_value(depth, Options, 1000)
    },
    UpdateInterval = proplists:get_value(update_interval, Options, 60),
    timer:apply_interval(UpdateInterval * 1000, gen_server, cast, [self(), update]),
    {ok, State}.

handle_cast(update, State=#state{terminal=Terminal, security=Security, period=Period, depth=Depth, last_bar=LastBar}) ->
    Strategy = State#state.strategy,
    StrategyState = State#state.strategy_state,
    TerminalState = trade_terminal:get_state(State#state.terminal),
    case trade_terminal:get_history(Terminal, Security, Period, 1, true) of
        {ok, [LastBar]} -> {noreply, State};
        {ok,  [NewBar]} ->
            NewState = State#state{last_bar=NewBar},
            {ok, History} = trade_terminal:get_history(Terminal, Security, Period, Depth, true),
            {noreply, handle_signal( Strategy:update(History, TerminalState, StrategyState), NewState )}
    end;

handle_cast(_, State) ->
    {noreply, State}.

get_stats(#state{terminal=Terminal, strategy=Strategy, strategy_state=StrategyState}) ->
    Money = trade_terminal_state:get_money( trade_terminal:get_state(Terminal) ),
    Stats = [{equity, Money}],
    case erlang:function_exported(Strategy, get_stats, 1) of
        false -> Stats;
        true  -> Stats ++ Strategy:get_stats(StrategyState)
    end.

stop(State=#state{strategy=Strategy, strategy_state=StrategyState}) ->
    TerminalState = trade_terminal:get_state(State#state.terminal),
    Strategy:stop(TerminalState, StrategyState).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_signal({Signal, StrategyState}, State) ->
    handle_signal(Signal, State#state{strategy_state=StrategyState});

handle_signal(NewLots, State=#state{terminal=Terminal, security=Security}) ->
    case trade_terminal:get_position_lots(Terminal, Security) of
        Lots when Lots  <  NewLots -> buy(NewLots-Lots, State);
        Lots when Lots  >  NewLots -> sell(Lots-NewLots, State);
        Lots when Lots =:= NewLots -> State
    end.

buy(Lots, State=#state{name=Name, terminal=Terminal, security=Security}) ->
    lager:info("strategy ~p: equity: ~p", [Name, trade_terminal:get_money(Terminal)]),
    case trade_terminal:buy_order(Terminal, Security, Lots) of
        {ok, ReqID} ->
            lager:info("strategy ~p: buying  ~p lots of ~p: ~p", [Name, Lots, Security, ReqID]);
        {error, Error} when is_list(Error) ->
            lager:warning("strategy ~p: buying  ~p lots of ~p: '~ts'", [Name, Lots, Security, Error]);
        {error, Error} ->
            lager:warning("strategy ~p: buying  ~p lots of ~p: ~p", [Name, Lots, Security, Error])
    end,
    State.

sell(Lots, State=#state{name=Name, terminal=Terminal, security=Security}) ->
    lager:info("strategy ~p: equity: ~p", [Name, trade_terminal:get_money(Terminal)]),
    case trade_terminal:sell_order(Terminal, Security, Lots) of
        {ok, ReqID} ->
            lager:info("strategy ~p: selling ~p lots of ~p: ~p", [Name, Lots, Security, ReqID]);
        {error, Error} when is_list(Error) ->
            lager:warning("strategy ~p: selling ~p lots of ~p: '~ts'", [Name, Lots, Security, Error]);
        {error, Error} ->
            lager:warning("strategy ~p: selling ~p lots of ~p: ~p", [Name, Lots, Security, Error])
    end,
    State.
