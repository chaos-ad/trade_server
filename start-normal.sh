#!/bin/bash
cd "$( dirname "$0" )"
mkdir -p "logs/sasl"
export ERL_LIBS="deps/:${ERL_LIBS}"
exec erl -smp disable -pa ebin -boot start_sasl -config priv/example.config -s reloader -s trade -sname tradesrv@`hostname` -cookie cookie_monsta
