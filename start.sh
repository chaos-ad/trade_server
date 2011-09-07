#!/bin/bash
cd "$( dirname "$0" )"
echo Im here: `pwd`
mkdir -p "log/sasl"
exec erl -pa ebin deps/*/ebin -boot start_sasl -s reloader -s trade -config priv/example.config
