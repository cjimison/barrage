%%%-------------------------------------------------------------------
%%% @author Chris Jimison
%%% @copyright (c) 2013 Christopher Jimison
%%%
%%% Permission is hereby granted, free of charge, to any person 
%%% obtaining a copy of this software and associated documentation file 
%%% (the "Software"), to deal in the Software without restriction, 
%%% including without limitation the rights to use, copy, modify, merge 
%%% publish, distribute, sublicense, and/or sell copies of the Software 
%%% and to permit persons to whom the Software is furnished to do so, 
%%% subject to the following conditions:
%%% 
%%% The above copyright notice and this permission notice shall be 
%%% included in all copies or substantial portions of the Software.
%%% 
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, 
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF 
%%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND 
%%% NONINFRINGEMENT. 
%%% IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR AN 
%%% CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
%%% TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE 
%%% SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%% @doc
%%%
%%% @end
%%% Created : 2013-05-29 11:26:24.741667
%%%-------------------------------------------------------------------
-module(handler_http_commander).

%% Module callbacks
-export([init/3]).
-export([handle/2]).
-export([routes/0]).
-export([terminate/3]).

-define(HTTP_CONTENT_ENC, [{<<"content-encoding">>,<<"utf-8">>}]).

%%%===================================================================
%%% Module callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% init the handle
%%
%% @spec init(_Transport, Req, []) -> {ok, Req, undefined}
%% @end
%%--------------------------------------------------------------------
init(_Transport, Req, []) ->
    {ok, Req, undefined}.

%%--------------------------------------------------------------------
%% @doc
%% callback made by cowboy when this handle is envoked
%%
%% @spec handle(Req, State) -> {ok, Req, State}
%% @end
%%--------------------------------------------------------------------
handle(Req, State) ->
    {Method, Req2}  = cowboy_req:method(Req),
    {Path, Req3}    = cowboy_req:path(Req2),
    handle_named_request(Method, Path, Req3),
    {ok, Req3, State}.

%%--------------------------------------------------------------------
%% @doc
%% Array of value URL routes that this handler will accept
%%
%% @spec routes() -> [{url, barrage_commander_handler, []}] 
%% @end
%%--------------------------------------------------------------------
routes() ->
    [
        {"/commander/status",       handler_http_commander, []},
        {"/commander/set_network",  handler_http_commander, []},
        {"/commander/set_general",  handler_http_commander, []},
        {"/commander/set_gunners",  handler_http_commander, []},
        {"/commander/connect",      handler_http_commander, []},
        {"/commander/disconnect",   handler_http_commander, []}
    ].

%%--------------------------------------------------------------------
%% @doc
%% Shuts down the cowboy handle
%%
%% @spec terminate(_Reason, _Req, _State) -> ok 
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _Req, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_named_request(<<"GET">>, <<"/commander/status">>, Req) ->
    Cookie  = erlang:atom_to_binary(erlang:get_cookie(), utf8), 
    Node    = erlang:atom_to_binary(erlang:node(), utf8),
    Count   = list_to_binary(
                integer_to_list(commander_server:gunner_count())),
    GeneralA= commander_server:general(),
    General = erlang:atom_to_binary(GeneralA, utf8),
    Status  = get_commander_state(),
    JSON    = <<"{\"network\":\"",
                Cookie/binary,"\",\"commander\":\"",
                Node/binary,"\",\"general\":\"",
                General/binary,"\",\"gunners\":",
                Count/binary,",\"state\":\"",
                Status/binary,"\"}">>,
    cowboy_req:reply(200,?HTTP_CONTENT_ENC, JSON,Req);

handle_named_request(<<"POST">>, <<"/commander/set_network">>, Req) ->
    case cowboy_req:has_body(Req) of
        true ->

            {ok, Plans, Req2}           = cowboy_req:body_qs(Req),
            case proplists:get_value(<<"network">>, Plans) of
                undefined ->
                    cowboy_req:reply(200, ?HTTP_CONTENT_ENC,
                                    <<"{\"error\":\"no network name set\"}">>,
                                    Req2);
                CookieB ->
                    Cookie  = erlang:binary_to_atom(CookieB, utf8),
                    case commander_server:change_cookie(Cookie) of
                        ok ->
                            cowboy_req:reply(200, ?HTTP_CONTENT_ENC,
                                            <<"{\"error\":\"none\"}">>,
                                            Req2);
                        queued ->
                            cowboy_req:reply(200, ?HTTP_CONTENT_ENC,
                            <<"{\"error\":\"none\", \"status\":\"queued\"}">>,
                            Req2)
                    end
            end;
        false ->
            cowboy_req:reply(200,?HTTP_CONTENT_ENC,
                            <<"{\"error\":\"No Body\"}">>,Req)
    end;

handle_named_request(<<"POST">>, <<"/commander/set_general">>, Req) ->
    case cowboy_req:has_body(Req) of
        true ->

            {ok, Cmd, Req2} = cowboy_req:body_qs(Req),
            case proplists:get_value(<<"general">>, Cmd) of
                undefined ->
                    cowboy_req:reply(200, ?HTTP_CONTENT_ENC,
                                    <<"{\"error\":\"no general\"}">>,
                                    Req2);
                GeneralL ->
                    General = erlang:binary_to_atom(GeneralL, utf8),
                    case commander_server:change_general(General) of
                        ok ->
                            cowboy_req:reply(200, ?HTTP_CONTENT_ENC,
                                            <<"{\"error\":\"none\"}">>,
                                            Req2);
                        queued ->
                            cowboy_req:reply(200, ?HTTP_CONTENT_ENC,
                            <<"{\"error\":\"none\", \"status\":\"queued\"}">>,
                            Req2)
                    end
            end;
        false ->
            cowboy_req:reply(200,?HTTP_CONTENT_ENC,
                            <<"{\"error\":\"No Body\"}">>,Req)
    end;

handle_named_request(<<"POST">>, <<"/commander/set_gunners">>, Req) ->
    case cowboy_req:has_body(Req) of
        true ->
            {ok, Cmd, Req2}  = cowboy_req:body_qs(Req),
            case proplists:get_value(<<"gunners">>, Cmd) of
                undefined ->
                    cowboy_req:reply(200, ?HTTP_CONTENT_ENC,
                                    <<"{\"error\":\"No gunners set\"}">>,
                                    Req2);
                Gunners ->
                    Val = 
                    case Gunners of 
                        Gunners when is_integer(Gunners) -> Gunners;
                        Gunners when is_binary(Gunners) -> erlang:binary_to_integer(Gunners)
                    end,

                    case commander_server:change_gunner_count(Val) of
                        ok ->
                            cowboy_req:reply(200, ?HTTP_CONTENT_ENC,
                                            <<"{\"error\":\"none\"}">>,
                                            Req2);
                        queued ->
                            cowboy_req:reply(200, ?HTTP_CONTENT_ENC,
                            <<"{\"error\":\"none\", \"status\":\"queued\"}">>,
                            Req2);
                        R ->
                            io:format("WTF ~p~n", [R])
                    end
            end;
        false ->
            cowboy_req:reply(200,?HTTP_CONTENT_ENC,
                            <<"{\"error\":\"No Body\"}">>,Req)
    end;

handle_named_request(<<"GET">>, <<"/commander/connect">>, Req) ->
    case commander_server:connect() of
        ok ->
            cowboy_req:reply(200, ?HTTP_CONTENT_ENC,
                            <<"{\"error\":\"none\"}">>,
                            Req);
        queued ->
            cowboy_req:reply(200, ?HTTP_CONTENT_ENC,
            <<"{\"error\":\"none\", \"status\":\"queued\"}">>,
            Req)
    end;

handle_named_request(<<"GET">>, <<"/commander/disconnect">>, Req) ->
    case commander_server:disconnect() of
        ok ->
            cowboy_req:reply(200, ?HTTP_CONTENT_ENC,
                            <<"{\"error\":\"none\"}">>,
                            Req);
        queued ->
            cowboy_req:reply(200, ?HTTP_CONTENT_ENC,
            <<"{\"error\":\"none\", \"status\":\"queued\"}">>,
            Req);
        _ ->
            cowboy_req:reply(200, ?HTTP_CONTENT_ENC,
            <<"{\"error\":\"none\", \"status\":\"error\"}">>,
            Req)
    end;

handle_named_request(_, _, _Req) ->
    ok.

get_commander_state() ->
    case commander_server:is_connected() of
        true ->
            <<"connected">>;
        false ->
            <<"disconnected">>
    end.
