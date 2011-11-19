-module(trade_terminal_starter).
-behaviour(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% gen_server callbacks:
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% api:
-export([start_link/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {pid, name, args, max, period, timeout, history}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Name, Options) ->
    gen_server:start_link(?MODULE, {Name, Options}, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init({Name, Options}) ->
    process_flag(trap_exit, true),
    gen_server:cast(self(), restart),
    {ok, #state{
        pid     = undefined,
        name    = Name,
        args    = Options,
        max     = proplists:get_value(restart_max, Options, 5),
        period  = proplists:get_value(restart_period, Options, 60),
        timeout = proplists:get_value(restart_timeout, Options, 5),
        history = []
    }}.

handle_call({stop, Reason}, _, State) ->
    {stop, Reason, ok, State};

handle_call(_, _, State) ->
    {noreply, ok, State}.

handle_cast(restart, State=#state{name=Name, args=Args}) ->
    lager:info("Starting terminal '~p'...", [Name]),
        case trade_terminal:start_link(Name, Args) of
            {ok, Pid} ->
            lager:info("Terminal '~p' started: ~p", [Name, Pid]),
            {noreply, State#state{pid=Pid}};
        {error, {str, Error}} ->
            lager:error("Failed to start terminal '~p': \"~ts\"", [Name, Error]),
            schedule_restart(State);
        {error, Error} ->
            lager:error("Failed to start terminal '~p': ~p", [Name, Error]),
            schedule_restart(State)
    end;

handle_cast(_, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, Reason}, State=#state{pid=Pid, name=Name}) ->
    lager:error("Terminal '~p' stopped with reason: ~p", [Name, Reason]),
    schedule_restart(State);

handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

schedule_restart(State=#state{name=Name}) ->
    Now     = trade_utils:universal_time(),
    NewHist = lists:filter(fun(Time) -> Time > Now - State#state.period end, State#state.history),
    case length(NewHist) < State#state.max of
        true ->
            lager:info("Restarting terminal '~p' after ~B seconds...", [Name, State#state.timeout]),
            timer:apply_after(State#state.timeout*1000, gen_server, cast, [self(), restart]),
            {noreply, State#state{pid=undefined, history=[Now|NewHist]}};
        false ->
            lager:error("Restart limit exceeded for terminal '~p', stopping", [Name]),
            {stop, {error, restart_limit_exceeded}}
    end.
