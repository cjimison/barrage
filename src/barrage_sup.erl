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

-module(barrage_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
get_props(true, true) ->
    [
        {tag0, {reloader, start_link, []}, permanent, 10000, worker, [reloader_server]},
        {tag1, {barrage_general, start_link, []}, permanent, 10000, worker, [barrage_general]},
        {tag2, {barrage_commander, start_link, []}, permanent, 10000, worker, [barrage_commander]}
    ]; 
get_props(true, false) ->
    [
        {tag0, {reloader, start_link, []}, permanent, 10000, worker, [reloader_server]},
        {tag1, {barrage_general, start_link, []}, permanent, 10000, worker, [barrage_general]}
    ]; 
get_props(false, true) ->
    [
        {tag0, {reloader, start_link, []}, permanent, 10000, worker, [reloader_server]},
        {tag2, {barrage_commander, start_link, []}, permanent, 10000, worker, [barrage_commander]}
    ]; 
get_props(_Gen, _Com) ->
    [
        {tag0, {reloader, start_link, []}, permanent, 10000, worker, [reloader_server]}
    ].

init([]) ->
    [{_, General}]      = ets:lookup(barrage, enable_general),
    [{_, Commander}]    = ets:lookup(barrage, enable_commander),
    Props               = get_props(General, Commander),
    {ok, { {one_for_one, 5, 10}, Props }}.

