#!/usr/bin/env escript
%% -*- erlang -*-
%%! -sname tradeadm@`hostname` -cookie trade_cookie

main(_) ->
    Host = list_to_atom("tradesrv@" ++ net_adm:localhost()),
    io:format("Stopping daemon: "),
    Res = rpc:call(Host, init, stop, []),
    io:format("~p~n", [Res]).


