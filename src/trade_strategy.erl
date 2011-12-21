-module(trade_strategy).
-behaviour(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% gen_server callbacks:
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% api:
-export([start_link/4, stop/1, stop/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([behaviour_info/1]).

behaviour_info(callbacks) -> [{start, 3}, {stop, 1}];
behaviour_info(_) -> undefined.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {name, term, module, opts, data}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Name, Terminal, Module, Options) ->
    gen_server:start_link(?MODULE, {Name, Terminal, Module, Options}, []).

stop(Pid) ->
    stop(Pid, normal).

stop(Pid, Reason) ->
    gen_server:call(Pid, {stop, Reason}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init({Name, Terminal, Module, Options}) ->
    gproc:nb_wait({n,l,Terminal}),
    lager:info("Strategy '~p': awaiting", [Name]),
    {ok, #state{name=Name, term=Terminal, module=Module, opts=Options}}.

handle_call({stop, Reason}, _, State) ->
    {stop, Reason, ok, State};

handle_call(Msg, From, State=#state{module=Module, data=Data}) ->
    case has_function(Module, handle_call, 3) of
        true  ->
            case Module:handle_call(Msg, From, Data) of
                {noreply,      NewData} -> {noreply,      State#state{data=NewData}};
                {reply, Reply, NewData} -> {reply, Reply, State#state{data=NewData}};
                {stop, Reason, NewData} -> {stop, Reason, State#state{data=NewData}}
            end;
        false ->
            {reply, {error, invalid_request}, State}
    end.

handle_cast(Msg, State=#state{module=Module, data=Data}) ->
    case has_function(Module, handle_cast, 2) of
        true  ->
            case Module:handle_cast(Msg, Data) of
                {noreply, NewData} -> {noreply, State#state{data=NewData}};
                {stop, Reason, NewData} -> {stop, Reason, State#state{data=NewData}}
            end;
        false ->
            {noreply, State}
    end.

handle_info({gproc, _, registered, {{n,l,Terminal},_,_}}, State=#state{name=Name, term=Terminal, module=Module, opts=Opts}) ->
    gproc:add_local_name(Name),
    gproc:monitor({n,l,Terminal}),
    {ok, Data} = Module:start(Name, Terminal, Opts),
    lager:info("Strategy '~p': started", [Name]),
    {noreply, State#state{data=Data}};

handle_info({gproc, unreg, _, {n,l,Terminal}}, State=#state{term=Terminal}) ->
    {stop, normal, State};

handle_info(Msg, State=#state{module=Module, data=Data}) ->
    case has_function(Module, handle_info, 2) of
        true  ->
            case Module:handle_info(Msg, Data) of
                {noreply, NewData} -> {noreply, State#state{data=NewData}};
                {stop, Reason, NewData} -> {stop, Reason, State#state{data=NewData}}
            end;
        false ->
            {noreply, State}
    end.

terminate(_, #state{name=Name, module=Module, data=Data}) ->
    Module:stop(Data),
    lager:info("Strategy '~p': stopped", [Name]),
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

has_function(Module, Function, Arity) ->
    erlang:function_exported(Module, Function, Arity).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
