-module(trade).

-export([start/0, stop/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
    application:load(?MODULE),
    ensure_deps_started(),
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE),
    ensure_deps_stopped().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ensure_started(App) ->
    case application:start(App) of
        ok                              -> ok;
        {error, {already_started, App}} -> ok
    end.

ensure_stopped(App) ->
    case application:stop(App) of
        ok                              -> ok;
        {error, E}                      -> io:format("Error: ~p~n", [E])
    end.

ensure_deps_started() ->
    {ok, DepsList} = application:get_key(?MODULE, applications),
    lists:foreach( fun ensure_started/1, DepsList ).

ensure_deps_stopped() ->
    {ok, DepsList} = application:get_key(?MODULE, applications),
    lists:foreach( fun ensure_stopped/1, DepsList ).
