#!/bin/sh
./rebar compile
erl -name commander@10.0.1.5 -setcookie barrage_cluster -pa ebin deps/*/ebin -s barrage -eval "io:format(\"Release HELL!!!~n\")."
