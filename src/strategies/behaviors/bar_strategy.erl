-module(bar_strategy).
-behaviour(gen_server).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% gen_server callbacks:
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([behaviour_info/1]).

behaviour_info(callbacks) -> [{start, 2}, {stop, 1}, {update, 2}];
behaviour_info(_) -> undefined.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {term, mod, data, symbol, period, depth}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Module, Terminal, Options) ->
    gen_server:start_link(?MODULE, {Terminal, Module, Options}, []).

update(Pid) ->
    gen_server:call(Pid, update, infinity).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init({Terminal, Module, Options}) ->
    {ok, Data} = Module:start(Terminal, Options),
    {ok, #state{
        term     = Terminal,
        mod      = Module,
        data     = Data,
        depth    = proplists:get_value(depth, Options, 1000),
        symbol   = proplists:get_value(symbol, Options),
        period   = proplists:get_value(period, Options)
    }}.

handle_call(update, _, State=#state{term=Term, mod=Mod, data=Data, symbol=Symbol, period=Period, depth=Depth}) ->
    History = trade_terminal:get_history(Term, Symbol, Period, Depth, true),
    {reply, ok, State#state{data=Mod:update(History, Data)}};

handle_call(_, _, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_, #state{mod=Module, data=Data}) ->
    Module:stop(Data).

code_change(_, State, _) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
