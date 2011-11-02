-module(gen_strategy).
-behavior(gen_server).

-export
([
    start_link/3,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{start, 1}, {stop, 1}, {update, 2}];

behaviour_info(_) ->
    undefined.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {module, account, strategy_state}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Module, Account, Args) ->
    gen_server:start_link(?MODULE, {Module, Account, Args}, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init({Module, Account, Args}) ->
    process_flag(trap_exit, true),
    lager:info("Starting strategy '~p' for account '~p'...", [Module, Account]),
    try Module:start(Args) of
        StrategyState ->
            lager:info("Starting strategy '~p' for account '~p': ok", [Module, Account]),
            {ok, #state{module=Module, account=Account, strategy_state=StrategyState}}
    catch
        _:Error ->
            lager:error("Failed to start strategy '~p' for account '~p': ~p", [Module, Account, Error]),
            {stop, {error, Error}}
    end.

handle_call(Something, _, State) ->
    lager:debug("Unexpected call received: ~p", [Something]),
    {reply, invalid_call, State}.

handle_cast(Something, State) ->
    lager:debug("Unexpected cast received: ~p", [Something]),
    {noreply, State}.

handle_info(Something, State) ->
    lager:debug("Unexpected info received: ~p", [Something]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

code_change(_, State, _) ->
    {ok, State}.

terminate(Reason, #state{module=Module, account=Account, strategy_state=StrategyState}) ->
    Module:stop(StrategyState),
    lager:info("Strategy '~p' for account '~p' stopped with reason: ~p", [Module, Account, Reason]),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
