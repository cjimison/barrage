%%%-------------------------------------------------------------------
%%% @author Chris Jimison
%%% @copyright (c) 2013 Christopher Jimison
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining 
%%% a copy of this software and associated documentation files 
%%% (the "Software"), to deal in the Software without restriction, 
%%% including without limitation the rights to use, copy, modify, merge, 
%%% publish, distribute, sublicense, and/or sell copies of the Software, 
%%% and to permit persons to whom the Software is furnished to do so, 
%%% subject to the following conditions:
%%% 
%%% The above copyright notice and this permission notice shall be 
%%% included in all copies or substantial portions of the Software.
%%% 
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, 
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF 
%%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
%%% IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY 
%%% CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, 
%%% TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE 
%%% SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%% @doc
%%%
%%% @end
%%% Created : 2013-04-02 11:31:39.401954
%%%-------------------------------------------------------------------

%% @doc GET echo handler.
-module(barrage_general_handler).

-export([init/3]).
-export([handle/2]).
-export([routes/0]).
-export([terminate/3]).

-define(HTTP_CONTENT_ENC, [{<<"content-encoding">>,<<"utf-8">>}]).

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
    Routes = [
        {"/general/status",             barrage_general_handler, []},
        {"/general/set_network",        barrage_general_handler, []},
        {"/general/orders",             barrage_general_handler, []},
        {"/general/commanders",         barrage_general_handler, []},
        {"/general/issue_order",        barrage_general_handler, []},
        {"/general/upload_behaviors",   barrage_general_handler, []},
        {"/general/upload_actions",     barrage_general_handler, []}
    ],
    Routes.

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
handle_named_request(<<"GET">>, <<"/general/status">>, Req) ->
    Cookie  = erlang:atom_to_binary(erlang:get_cookie(), utf8), 
    Node    = erlang:atom_to_binary(erlang:node(), utf8),
    JSON    = <<"{\"network\":\"",
                Cookie/binary,"\",\"general\":\"",
                Node/binary,"\"}">>,
    cowboy_req:reply(200,?HTTP_CONTENT_ENC, JSON, Req);

handle_named_request(<<"POST">>, <<"/general/set_network">>, Req) ->
    case cowboy_req:has_body(Req) of
        true ->
            {ok, [{Data, true}], Req2}  = cowboy_req:body_qs(Req),
            {Plans}                     = jiffy:decode(Data),
            case proplists:get_value(<<"network">>, Plans) of
                undefined ->
                    cowboy_req:reply(200, ?HTTP_CONTENT_ENC,
                                    <<"{\"error\":\"no network name set\"}">>,
                                    Req2);
                CookieB ->
                    Cookie  = erlang:binary_to_atom(CookieB, utf8),
                    case barrage_general:change_cookie(Cookie) of
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

handle_named_request(<<"GET">>, <<"/general/orders">>, Req) ->
    [{table_keys, Keys}] = ets:lookup(plans, table_keys),
    JSON = jiffy:encode(Keys),
    cowboy_req:reply(200,?HTTP_CONTENT_ENC, JSON, Req);

handle_named_request(<<"GET">>, <<"/general/commanders">>, Req) ->
    Commanders = barrage_general:get_commanders_info(), 
    JSON = jiffy:encode(Commanders),
    cowboy_req:reply(200,?HTTP_CONTENT_ENC, JSON, Req);

handle_named_request(<<"GET">>, <<"/general/issue_order">>, Req) ->
    {Order, Req2} = cowboy_req:qs_val(<<"order">>, Req), 
    Req3 = cowboy_req:compact(Req2),
    barrage_general:issue_http_order(Order, self()),
    blocker_loop(Req3);

handle_named_request(<<"POST">>,<<"/general/upload_behaviors">>,Req) ->
    case cowboy_req:has_body(Req) of
        true ->
            {ok, [{Data, true}], Req2}  = cowboy_req:body_qs(Req),
            Plans   = jiffy:decode(Data),
            ok      = barrage_parser:load_behavior_data(Plans),
            cowboy_req:reply(200, ?HTTP_CONTENT_ENC,
                            <<"{\"error\":\"none\"}">>,Req2);
        false ->
            cowboy_req:reply(200,?HTTP_CONTENT_ENC,
                            <<"{\"error\":\"No Body\"}">>,Req)
    end;

handle_named_request(<<"POST">>, <<"/general/upload_actions">>, Req) ->
    case cowboy_req:has_body(Req) of
        true ->
            {ok, [{Data, true}], Req2}  = cowboy_req:body_qs(Req),
            Actions = jiffy:decode(Data),
            ok      = barrage_parser:load_action_data(Actions),
            cowboy_req:reply(200,?HTTP_CONTENT_ENC,
                            <<"{\"error\":\"none\"}">>,Req2);
        false ->
            cowboy_req:reply(200,?HTTP_CONTENT_ENC,
                            <<"{\"error\":\"No Body\"}">>,Req)
    end;
    
handle_named_request(_, _, Req) ->
    cowboy_req:reply(405, Req).

blocker_loop(Req) ->
    receive
        {done, Data} ->
            % lets stringify this data
            JSONData = jiffy:encode({dict:to_list(Data)}),
            cowboy_req:reply(200, ?HTTP_CONTENT_ENC, JSONData, Req);
        _ ->
            blocker_loop(Req) 
    end.

