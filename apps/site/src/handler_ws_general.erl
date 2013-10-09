%%%-------------------------------------------------------------------
%%% @author Chris Jimison
%%% @copyright (C) 2013, Not Rigged Games LLC
%%% @doc
%%%
%%% @end
%%% Created : 2013-05-31 12:47:31.011998
%%%-------------------------------------------------------------------
-module(handler_ws_general).

-behaviour(cowboy_websocket_handler).

%% Module callbacks
-export([routes/0]).
-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-export([send/2]).

%%%===================================================================
%%% Module callbacks
%%%===================================================================

routes() ->
    [{"/general/streaming", handler_ws_general, []}].

send(PID, Data) ->
    erlang:start_timer(0, PID, jiffy:encode(Data)).

%%--------------------------------------------------------------------
%% @doc
%% init the handle
%%
%% @spec init(_Transport, Req, []) -> {ok, Req, undefined}
%% @end
%%--------------------------------------------------------------------
init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    general_server:reg_stream(self()),
    {ok, Req, undefined_state}.

%%--------------------------------------------------------------------
%% @doc
%% callback when the socket is closed
%%
%% @spec websocket_handle(_Data, Req, State) -> {ok, Req, State}
%% @end
%%--------------------------------------------------------------------
websocket_handle({text, Msg}, Req, State) ->
    {Rez} = jiffy:decode(Msg), 
    case proplists:get_value(<<"cmd">>, Rez) of
        <<"order">> ->
            Order = proplists:get_value(<<"behavior">>, Rez),
            general_server:issue_order(Order),
            {reply, {text, <<"{\"error\":\"none\"}">>}, Req, State};
        _ ->
            {reply, {text, <<"{\"error\":\"unknown command\"}">>}, Req, State}
    end;

websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

%%--------------------------------------------------------------------
%% @doc
%% callback when the socket is closed
%%
%% @spec websocket_info(_Info, Req, State) -> {ok, Req, State}
%% @end
%%--------------------------------------------------------------------
websocket_info({timeout, _Ref, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

%%--------------------------------------------------------------------
%% @doc
%% callback when the socket is closed
%%
%% @spec websocket_terminate(_Reason, _Req, _State) -> ok
%% @end
%%--------------------------------------------------------------------
websocket_terminate(_Reason, _Req, _State) ->
    general_server:unreg_stream(self()),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================


