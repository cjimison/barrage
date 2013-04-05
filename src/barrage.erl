%%%-------------------------------------------------------------------
%%% @author Chris Jimison
%%% @copyright (C) 2013, Not Rigged Games LLC
%%% @doc
%%%     This module is created just to launch the to call
%%%     my application start
%%%
%%%     NOTE:
%%%
%%%     This code will NOT be called in a production build so don't
%%%     put anything in here that is critical to the execution of
%%%     your system!
%%% @end
%%% Created : 2013-04-02 11:23:14.344232
%%%-------------------------------------------------------------------
-module(barrage).

%% API
-export([start/0]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start() ->
    ok = application:start(barrage).
