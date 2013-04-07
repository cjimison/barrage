
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
        {tag0, {barrage_general, start_link, []}, 
            permanent, 10000, worker, [barrage_general]},
        {tag1, {barrage_commander, start_link, []}, 
            permanent, 10000, worker, [barrage_commander]}
    ]; 
get_props(true, false) ->
    [
        {tag0, {barrage_general, start_link, []}, 
            permanent, 10000, worker, [barrage_general]}
    ]; 
get_props(false, true) ->
    [
        {tag1, {barrage_commander, start_link, []}, 
            permanent, 10000, worker, [barrage_commander]}
    ]; 
get_props(_Gen, _Com) ->
    [].

init([]) ->
    [{_, General}]      = ets:lookup(barrage, enable_general),
    [{_, Commander}]    = ets:lookup(barrage, enable_general),
    Props               = get_props(General, Commander),
    {ok, { {one_for_one, 5, 10}, Props }}.

