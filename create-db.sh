#!/bin/bash
cd `dirname $0`
exec erl -pa $PWD/ebin $PWD/deps/*/ebin -s trade_db create_db -config priv/app.config -s init stop -noshell -sname tradesrv@`hostname`

