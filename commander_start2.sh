#!/bin/sh
./rebar compile
erl -name commander2@127.0.0.1 -setcookie barrage_cluster -pa `pwd`/ebin deps/*/ebin -s barrage -eval "io:format(\"Ready to unleash HELL!!!~n\")."
