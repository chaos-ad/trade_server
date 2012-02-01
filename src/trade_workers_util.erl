-module(trade_workers_util).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_threaded(Options, Threads) ->
    run_jobs(lists:map(fun make_job/1, Options), Threads).

make_job({Symbol, Period, From, Strategy, Options, TestOptions}) ->
    fun() -> trade_tester:start(Symbol, Period, From, Strategy, Options, TestOptions) end.

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%