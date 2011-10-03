-module(trade_transaq_srv).

-export([start_link/0, stop/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    {ok, Options} = application:get_env(transaq_srv),
    Host = proplists:get_value(host, Options),
    Port = proplists:get_value(port, Options),
    ServerOptions = [{name, trade_transaq_srv}, {port, Port}, {ip, Host}, {loop, fun(Sock) -> accept(Sock) end}],
    error_logger:info_msg("accepting transaq connections at ~s:~B~n", [Host, Port]),
    mochiweb_socket_server:start(ServerOptions).

stop() ->
    mochiweb_socket_server:stop(trade_transaq_srv).

accept(Socket) ->
    trade_transaq_conn:start(Socket).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
