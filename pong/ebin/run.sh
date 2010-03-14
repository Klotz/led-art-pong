#!/bin/sh

# run.sh
#
# load and start the application
#
/usr/local/bin/erl -boot start_sasl -config pong -pa ebin -eval "application:load(pong), application:start(pong)"
