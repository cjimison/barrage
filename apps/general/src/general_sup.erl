
-module(general_sup).

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

init([]) ->
    case application:get_env(general, enabled) of
        {ok, true} ->
            {ok, { {one_for_one, 5, 10}, [?CHILD(general_server, worker)]}};
        {ok, false} ->
            {ok, { {one_for_one, 5, 10}, []}};
        undefined ->
            {ok, { {one_for_one, 5, 10}, []}}
    end.

