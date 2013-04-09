#!/bin/sh
./rebar compile
erl -name commander -setcookie barrage_cluster -pa `pwd`/ebin deps/*/ebin -s barrage -eval "io:format(\"Ready to unleash HELL!!!~n\")."
