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
-export([terminate/3]).

blocker_loop(Req) ->
    receive
        {done, Data} ->
            % lets stringify this data
            JSONData = jiffy:encode({dict:to_list(Data)}),
            cowboy_req:reply(200, [{<<"content-encoding">>, <<"utf-8">>}], JSONData, Req);
        _ ->
            blocker_loop(Req) 
    end.


init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req2}  = cowboy_req:method(Req),
    {Path, Req3}    = cowboy_req:path(Req),
    handle_named_request(Method, Path, Req3),
    {ok, Req2, State}.

handle_named_request(<<"GET">>, <<"/orders">>, Req) ->
    [{table_keys, Keys}] = ets:lookup(plans, table_keys),
    JSON = jiffy:encode(Keys),
    cowboy_req:reply(200,[{<<"content-encoding">>,<<"utf-8">>}],JSON,Req);

handle_named_request(<<"GET">>, <<"/commanders">>, Req) ->
    Commanders = barrage_general:get_commanders_info(), 
    JSON = jiffy:encode(Commanders),
    cowboy_req:reply(200,[{<<"content-encoding">>,<<"utf-8">>}],JSON,Req);

handle_named_request(<<"GET">>, <<"/issue_order">>, Req) ->
    {Order, Req2} = cowboy_req:qs_val(<<"order">>, Req), 
    Req3 = cowboy_req:compact(Req2),
    barrage_general:issue_http_order(Order, self()),
    blocker_loop(Req3);

handle_named_request(<<"POST">>, <<"/upload_behaviors">>, Req) ->
    case cowboy_req:has_body(Req) of
        true ->
            {ok, [{Data, true}], Req2}  = cowboy_req:body_qs(Req),
            Plans                       = jiffy:decode(Data),
            ok                          = barrage_parser:load_behavior_data(Plans),
            cowboy_req:reply(200,[{<<"content-encoding">>,<<"utf-8">>}],<<"{\"error\":\"none\"}">>,Req2);
        false ->
            cowboy_req:reply(400,[{<<"content-encoding">>,<<"utf-8">>}],<<"{\"error\":\"No Body\"}">>,Req)
    end;

handle_named_request(<<"POST">>, <<"/upload_actions">>, Req) ->
    case cowboy_req:has_body(Req) of
        true ->
            {ok, [{Data, true}], Req2}  = cowboy_req:body_qs(Req),
            Actions                     = jiffy:decode(Data),
            ok                          = barrage_parser:load_action_data(Actions),
            cowboy_req:reply(200,[{<<"content-encoding">>,<<"utf-8">>}],<<"{\"error\":\"none\"}">>,Req2);
        false ->
            cowboy_req:reply(400,[{<<"content-encoding">>,<<"utf-8">>}],<<"{\"error\":\"No Body\"}">>,Req)
    end;
    
handle_named_request(_, _, Req) ->
    cowboy_req:reply(405, Req).

terminate(_Reason, _Req, _State) ->
	ok.
