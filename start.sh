#!/bin/bash
cd "$( dirname "$0" )"
echo Im here: `pwd`
mkdir -p "logs/sasl"
exec erl -smp disable -pa ebin deps/*/ebin -boot start_sasl -s reloader -s trade -config priv/example.config
