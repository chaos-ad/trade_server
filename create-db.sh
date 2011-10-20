#!/bin/bash
cd "$( dirname "$0" )"
export ERL_LIBS="deps/:${ERL_LIBS}"
exec erl -pa ebin -s trade_db create_db -config priv/example.config -s init stop -noshell -sname tradesrv@`hostname` -cookie cookie_monsta

