#!/bin/sh
./rebar compile
erl -name general@localhost -setcookie barrage_cluster erl -pa `pwd`/ebin deps/*/ebin -s barrage -eval "io:format(\"Started\")."
