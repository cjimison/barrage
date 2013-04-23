#!/bin/sh
./rebar compile
erl -name general@127.0.0.1 -setcookie barrage_cluster -pa `pwd`/ebin deps/*/ebin -s barrage -eval "io:format(\"Started~n\")."
