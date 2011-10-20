#!/usr/bin/env escript
%% -*- erlang -*-
%%! -sname admin@`hostname` -cookie cookie_monsta

main(_) ->
    Host = list_to_atom("tradesrv@" ++ net_adm:localhost()),
    io:format("Stopping daemon: "),
    Res = rpc:call(Host, init, stop, []),
    io:format("~p~n", [Res]).


