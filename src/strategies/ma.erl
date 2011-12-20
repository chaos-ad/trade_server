-module(ma).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start/1, stop/1, update/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {
    p1,         %% период первой MA
    p2,         %% период второй MA
    lots,       %% Сколько лотов покупать
    hold,       %% Продавать через hold баров
    days_in_pos=0
}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Options) ->
    #state{
        p1   = proplists:get_value(p1, Options,  10),
        p2   = proplists:get_value(p2, Options, 100),
        lots = proplists:get_value(lots, Options, 1),
        hold = proplists:get_value(hold, Options, 5)
    }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Нет сделок:
update(History, _Terminal, State=#state{days_in_pos=0}) ->
    case length(History) of
        N when N < State#state.p1 -> {0, State};
        N when N < State#state.p2 -> {0, State};
        _ ->
            MA1 = trade_utils:weighted_average(trade_utils:close(lists:sublist(History, State#state.p1))),
            MA2 = trade_utils:weighted_average(trade_utils:close(lists:sublist(History, State#state.p2))),
            OLD1= trade_utils:weighted_average(trade_utils:close(lists:sublist(History, 2, State#state.p1))),
            OLD2 = trade_utils:weighted_average(trade_utils:close(lists:sublist(History, 2, State#state.p2))),

            %% Если MA1 пересекла MA2 снизу вверх: покупаем
            case MA1 > MA2 andalso OLD1 < OLD2 of
                true  -> {0, State#state{days_in_pos=1}};
                false -> {0, State}
            end
    end;

%% Мы продержали сделку N дней из M необходимых: держим дальше
update(_History, _Terminal, State=#state{days_in_pos=N, hold=M, lots=Lots}) when N =< M ->
    {round(N/M*Lots), State#state{days_in_pos=N+1}};


%% Мы продержали сделку M дней из M необходимых: закрываем
update(_History, _Terminal, State=#state{days_in_pos=N, hold=M}) when N > M ->
    {0, State#state{days_in_pos=0}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

stop(_) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

