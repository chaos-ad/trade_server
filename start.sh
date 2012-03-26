#!/bin/bash
cd `dirname $0`
exec erl -pa ebin deps/*/ebin -boot start_sasl -config priv/app.config -sname tradesrv@`hostname` -cookie trade_cookie -s trade_reloader -s trade_server $@
