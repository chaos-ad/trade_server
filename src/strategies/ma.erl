-module(ma).
-behavior(simple_strategy).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start_link/1]).
-export([start/1, stop/1, update/2]).

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

start_link(Options) ->
    gen_strategy:start_link(?MODULE, Options).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Options) ->
    #state{
        terminal    = proplists:get_value(terminal, Options),
        symbol      = proplists:get_value(symbol,   Options),
        lots        = proplists:get_value(lots,     Options, 1),
        p1          = proplists:get_value(p1,       Options, 10),
        p2          = proplists:get_value(p2,       Options, 100),
        hold        = proplists:get_value(hold,     Options, 5)
    }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Мы продержали сделку M дней из M необходимых: закрываем
update(_History, State=#state{in_pos=M, hold=M, symbol=Symbol, lots=Lots, terminal={Term, Pid}}) ->
    ok = Term:sell_order(Pid, Symbol, Lots),
    State#state{in_pos=0};

%% Мы продержали сделку N дней из M необходимых: держим дальше
update(_History, State=#state{in_pos=N, hold=M}) when N < M ->
    State#state{in_pos=N+1};

%% Нет сделок:
update(History, State=#state{in_pos=0, symbol=Symbol, lots=Lots, terminal={Term, Pid}}) ->
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
                    ok = Term:buy_order(Pid, Symbol, Lots),
                    State#state{in_pos=1};
                false ->
                    State
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

stop(_) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
