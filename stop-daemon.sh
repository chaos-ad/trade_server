#!/usr/bin/env escript
%% -*- erlang -*-
%%! -sname admin@`hostname` -cookie cookie_monsta

main(_) ->
    Host = list_to_atom("tradesrv@" ++ net_adm:localhost()),
    pong = net_adm:ping(Host),
    rpc:call(Host, init, stop, []).

