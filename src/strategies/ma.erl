-module(ma).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start/1, stop/1, update/2]).

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

%% Мы продержали сделку M дней из M необходимых: закрываем
update(_, State=#state{days_in_pos=N, hold=M}) when N =/= 0, N =:= M ->
    {0, State#state{days_in_pos=0}};

%% Мы продержали сделку N дней из M необходимых: держим дальше
update(_, State=#state{days_in_pos=N, hold=M, lots=Lots}) when N =/= 0, N < M ->
    {Lots, State#state{days_in_pos=N+1}};

%% Нет сделок:
update(History, State=#state{days_in_pos=0, lots=Lots}) ->
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
                true  -> {Lots, State#state{days_in_pos=1}};
                false -> {0, State}
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

stop(_) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile(export_all).

test(Threads) ->
    Jobs =
    [ fun() -> test(P1, P2, H) end ||
        P1 <- lists:seq(10, 100, 10),
        P2 <- lists:seq(10, 100, 10),
        H <- lists:seq(1, 1), P1 < P2
%         P1 <- lists:seq(5, 100, 5),
%         P2 <- lists:seq(5, 100, 5),
%         H <- lists:seq(1, 10), P1 < P2
    ],
    Results = run_jobs(Jobs, Threads),
    Equities = lists:map(fun({P1, P2, H, R}) -> {P1, P2, H, element(2, R)} end, Results),
    SortedEquities = lists:reverse(lists:keysort(4, Equities)),
    lists:foreach( fun({P1, P2, H, M}) -> io:format("~p: p1=~p, p2=~p h=~p~n", [M, P1, P2, H]) end, SortedEquities ).

test(P1, P2, H) ->
    io:format("Testing: P1 = ~p, P2 = ~p, H = ~p...~n", [P1, P2, H]),
    Res = trade_tester:test("GAZP", 5, {2011, 1, 1}, ma, [{p1, P1}, {p2, P2}, {hold, H}], [{money, 10000}]),
    io:format("Testing: P1 = ~p, P2 = ~p, H = ~p: done~n", [P1, P2, H]),
    {P1, P2, H, Res}.

run_jobs(Jobs, Threads) when is_integer(Threads) ->
    Workers = spawn_workers(Threads),
    Res = run_jobs(Jobs, Workers),
    stop_workers(Workers),
    Res;

run_jobs(Jobs, Workers) when is_list(Workers) ->
    run_jobs_loop(Jobs, queue:from_list(Workers), [], []).

run_jobs_loop([], _, [], Acc) -> lists:reverse(Acc);
run_jobs_loop([], Idle, Busy, Acc) -> wait_for_result([], Idle, Busy, Acc);

run_jobs_loop(Jobs, Idle, Busy, Acc) ->
    case queue:out(Idle) of
        {empty, _} ->
            wait_for_result(Jobs, Idle, Busy, Acc);
        {{value, Pid}, NewIdle} ->
            Pid ! {job, self(), hd(Jobs)},
            run_jobs_loop(tl(Jobs), NewIdle, [Pid|Busy], Acc)
    end.

wait_for_result(Jobs, Idle, Busy, Acc) ->
    receive
        {job, Pid, Res} ->
            run_jobs_loop(Jobs, queue:in(Pid, Idle), lists:delete(Pid, Busy), [Res|Acc])
    end.

spawn_workers(N) ->
    [ spawn_link( fun worker/0 ) || _ <- lists:seq(1, N) ].

stop_workers(Workers) ->
    [ Pid ! stop || Pid <- Workers ].


worker() ->
    receive
        {job, Pid, Job} ->
            Pid ! {job, self(), catch Job()},
            worker();
        stop ->
            ok
    end.
