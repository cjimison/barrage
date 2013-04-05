#!/bin/sh
./rebar compile
erl -pa ebin deps/*/ebin -s barrage \
    -eval "io:format(\"Release HELL!!!~n\")."
