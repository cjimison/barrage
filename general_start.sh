#!/bin/sh
./rebar compile
erl -name general@10.0.1.4 -setcookie barrage_cluster -pa ebin deps/*/ebin -s barrage -eval "io:format(\"Release HELL!!!~n\")."
