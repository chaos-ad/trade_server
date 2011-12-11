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

-record(state, {term, mod, data, symbol, period, depth, last_bar}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Module, Terminal, Options) ->
    gen_server:start_link(?MODULE, {Terminal, Module, Options}, []).

update(Pid) ->
    gen_server:call(Pid, update).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init({Terminal, Module, Options}) ->
    {Symbol, Period, Depth, Data} = Module:start(Terminal, Options),
    {ok, #state{
        mod    = Module,
        term   = Terminal,
        data   = Data,
        symbol = Symbol,
        period = Period,
        depth  = Depth
    }}.

handle_call(update, _, State=#state{mod=Mod, term=Terminal, data=Data, symbol=Symbol, period=Period, last_bar=LastBar}) ->
    case trade_terminal:get_history(Terminal, Symbol, Period, 1000, true) of
        [] -> {reply, ok, State};
        History ->
            case hd(History) =:= LastBar of
                true  -> {reply, ok, State};
                false -> {reply, ok, State#state{data=Mod:update(History, Data), last_bar=hd(History)}}
            end
    end;

handle_call(_, _, State) ->
    {noreply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_, #state{mod=Module, data=Data}) ->
    Module:stop(Data).

code_change(_, State, _) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
