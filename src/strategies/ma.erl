-module(ma).
-behavior(bar_strategy).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start_link/2, update/1]).
-export([start/2, stop/1, update/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {
    terminal,
    symbol,
    lots,       %% Сколько лотов покупать
    p1,         %% период первой MA
    p2,         %% период второй MA
    hold,       %% Продавать через hold баров
    in_pos=0
}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Terminal, Options) ->
    bar_strategy:start_link(?MODULE, Terminal, Options).

update(Pid) ->
    bar_strategy:update(Pid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Terminal, Options) ->
    Symbol      = proplists:get_value(symbol, Options),
    TimeFrame   = proplists:get_value(timeframe, Options),
    P1          = proplists:get_value(p1, Options,  10),
    P2          = proplists:get_value(p2, Options, 100),
    Lots        = proplists:get_value(lots, Options, 1),
    Hold        = proplists:get_value(hold, Options, 5),
    State       = #state{terminal=Terminal, symbol=Symbol, lots=Lots, p1=P1, p2=P2, hold=Hold},
    {Symbol, TimeFrame, max(P1, P2), State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Мы продержали сделку M дней из M необходимых: закрываем
update(History, State=#state{in_pos=N, hold=M, symbol=Symbol, lots=Lots, terminal=Pid}) when N =/= 0, N =:= M ->
    lager:debug("ma strategy: ~s: sell ~p of ~s", [time(History), Lots, Symbol]),
    ok = trade_terminal:sell_order(Pid, Symbol, Lots),
    State#state{in_pos=0};

%% Мы продержали сделку N дней из M необходимых: держим дальше
update(_, State=#state{in_pos=N, hold=M}) when N =/= 0, N < M ->
    State#state{in_pos=N+1};

%% Нет сделок:
update(History, State=#state{in_pos=0, symbol=Symbol, lots=Lots, terminal=Pid}) ->
    case length(History) of
        N when N < State#state.p1 -> State;
        N when N < State#state.p2-> State;
        _ ->
            MA1 = trade_utils:weighted_average(trade_utils:close(lists:sublist(History, State#state.p1))),
            MA2 = trade_utils:weighted_average(trade_utils:close(lists:sublist(History, State#state.p2))),
            OLD1= trade_utils:weighted_average(trade_utils:close(lists:sublist(History, 2, State#state.p1))),
            OLD2 = trade_utils:weighted_average(trade_utils:close(lists:sublist(History, 2, State#state.p2))),

            %% Если MA1 пересекла MA2 снизу вверх: покупаем
            case MA1 > MA2 andalso OLD1 < OLD2 of
                true  ->
                    lager:debug("ma strategy: ~s: buy  ~p of ~s", [time(History), Lots, Symbol]),
                    ok = trade_terminal:buy_order(Pid, Symbol, Lots),
                    State#state{in_pos=1};
                false ->
                    State
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

stop(_) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

time(History) ->
    trade_utils:to_datetimestr(element(1, hd(History))).