#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -config priv/app.config -s reloader -s trade -sname tradesrv@`hostname`
