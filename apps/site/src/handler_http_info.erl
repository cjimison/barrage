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
-module(handler_http_info).

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
        {"/info/type",  handler_http_info, []}
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
handle_named_request(<<"GET">>, <<"/info/type">>, Req) ->
    {ok, ComEnabled}= application:get_env(commander, enabled),
    {ok, GenEnabled}= application:get_env(general, enabled),
    
    Val             = case {ComEnabled, GenEnabled} of
                            {true, true} ->
                                <<"\"gen_com\"">>;
                            {true, _} ->
                                <<"\"com\"">>;
                            {_, true} ->
                                <<"\"gen\"">>;
                            {_, _} ->
                                <<"\"unknown\"">>
                     end,
                                

    JSON    = <<"{\"type\":", Val/binary, "}">>,
    cowboy_req:reply(200,?HTTP_CONTENT_ENC, JSON,Req);

handle_named_request(_, _, _Req) ->
    ok.

