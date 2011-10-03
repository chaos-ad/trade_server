#!/bin/bash
cd "$( dirname "$0" )"
echo Im here: `pwd`
exec erl -smp disable -pa ebin deps/*/ebin -s trade_db create_db -config priv/example.config -s init stop
