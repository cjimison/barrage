#!/bin/sh
./rebar compile
erl -name commander -setcookie barrage_cluster -pa ebin deps/*/ebin -s barrage -eval "io:format(\"Release HELL!!!~n\")."
