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


-module(site_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
build_routes() ->
    build_general_routes() ++ 
    build_commander_routes() ++ 
    handler_http_info:routes().

build_commander_routes() ->
    case application:get_env(commander, enabled) of
        {ok, true} ->
            handler_http_commander:routes();
        _ ->
            []
    end.

build_general_routes() ->
    case application:get_env(general, enabled) of
        {ok, true} ->
            handler_http_general:routes() ++ handler_ws_general:routes(); 
        _ ->
            []
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    {ok, WebPort} = application:get_env(site, web_port),
    Routes = build_routes() ++ 
                [{"/[...]", cowboy_static, [
                    {directory, {priv_dir, site, []}},
                    {mimetypes, [
                            {<<".html">>,   [<<"text/html">>]},
                            {<<".txt">>,    [<<"text/plain">>]},
                            {<<".css">>,    [<<"text/css">>]},
                            {<<".png">>,    [<<"image/png">>]},
                            {<<".js">>,     [<<"application/javascript">>]},
                            {<<".json">>,   [<<"application/json">>]}
                    ]}
                ]}], 

    Dispatch = cowboy_router:compile([{'_', Routes } ]),
    % this is a general so load up the correct end points
    {ok, _} = cowboy:start_http(http, 100, [{port, WebPort}], [
            {env, [{dispatch, Dispatch}]}
    ]),

    site_sup:start_link().

stop(_State) ->
    ok.
