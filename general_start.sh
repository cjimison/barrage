#!/bin/sh
./rebar compile
erl -name general -setcookie barrage_cluster -pa ebin deps/*/ebin -s barrage -eval "io:format(\"Release HELL!!!~n\")."
