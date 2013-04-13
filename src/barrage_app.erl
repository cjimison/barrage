%%%-------------------------------------------------------------------
%%% Copyright (c) 2013 Christopher Jimison
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
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% @author Chris Jimison
%%% @copyright (C) 2013, Not Rigged Games LLC
%%% @doc
%%%
%%% @end
%%% Created : 2013-04-02 11:31:39.401954
%%%-------------------------------------------------------------------
-module(barrage_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

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
    ok = inets:start(),
    Dispatch = cowboy_router:compile([
            {'_', [
                    {"/status",     barrage_general_handler, []},
                    {"/orders",     barrage_general_handler, []},
                    {"/commanders", barrage_general_handler, []},
                    {"/issue_order",barrage_general_handler, []},
                    {"/[...]", cowboy_static, [
                            {directory, {priv_dir, barrage, []}},
                            {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
		    ]} 
            ]}
    ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
            {env, [{dispatch, Dispatch}]}
    ]),

    case loadConfigTable() of
        ok ->
            case barrage_sup:start_link() of
                {ok, Pid} ->
                    {ok, Pid};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%%  Process all the behaviors and load them into an ets table.
%%  int he future I would like to make this modifiable
%%  but then I will need to make it work in a cluster....
%%
%% @spec process_plans(Plans) -> ok
%%      Plans = Array
%% @end
%%--------------------------------------------------------------------
process_plans([]) ->
    ok;
process_plans(Plans) ->
    [{Plan} | OtherPlans]   = Plans,
    Name                    = proplists:get_value(name, Plan),
    Tree                    = proplists:get_value(tree, Plan),
  
    [{table_keys, OldKeys}] = ets:lookup(plans, table_keys),
    NewKeys = [Name | OldKeys],
    ets:insert(plans, {table_keys, NewKeys}),
    ets:insert(plans, {Name, Tree}),
    process_plans(OtherPlans).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%  Process the Actions defined by the user
%%
%% @spec process_actions(Actions) -> ok
%%      Plans = Array
%% @end
%%--------------------------------------------------------------------
process_actions([]) ->
    ok;
process_actions(Actions) ->
    [{Action} | OtherActions]   = Actions,
    Name                        = proplists:get_value(name, Action),
    ets:insert(actions, {Name, Action}),
    process_actions(OtherActions).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%  Process the Config data
%%
%% @spec process_actions(Actions) -> ok
%%      Plans = Array
%% @end
%%--------------------------------------------------------------------
process_config([]) ->
    ok;
process_config(Configs) ->
    [{Config} | OtherConfigs]   = Configs,
    Type                        = proplists:get_value(type, Config),
    Args                        = proplists:get_value(args, Config),
    ets:insert(barrage, {type, Type}),
    case Type of
        general ->
            ets:insert(barrage, {enable_general, true}),
            URL = proplists:get_value(url, Args),
            ets:insert(barrage, {url, URL});
        commander ->
            ets:insert(barrage, {enable_commander, true}),
            Gunners = proplists:get_value(gunners, Args),
            General = proplists:get_value(general, Args),
            ets:insert(barrage, {general, General}),
            ets:insert(barrage, {gunners, Gunners})
    end,
    process_config(OtherConfigs).


%%--------------------------------------------------------------------
%% @private
%% @doc
%%  Loads all the config files that currently exist for the system
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
loadConfigTable()->
    BasePath        = code:priv_dir(barrage),
    {ok, Configs}   = file:consult(BasePath ++ "/barrage.config"), 
    {ok, Plans}     = file:consult(BasePath ++ "/behaviors.config"),
    {ok, Actions}   = file:consult(BasePath ++ "/actions.config"),
    
    % Now lets set parse the plans in order to build out an ets table
    ets:new(barrage, [set, named_table]),
    process_config(Configs),

    ets:new(plans, [set, named_table]),
    ets:insert(plans, {table_keys, []}),
    process_plans(Plans),

    ets:new(actions, [set, named_table]),
    process_actions(Actions).

    %try
    %    %{ok, BarrageStr}    = file:read_file("./priv/barrage.json"),
    %    %io:format("file read, now parse ~n"),
    %    %{BarrageConfig}     = jiffy:decode(BarrageStr),
    %    %parseBarrageConfig(BarrageConfig),

    %    %{ok, ActionsStr}    = file:read_file("config/actions.json"),
    %    %{ok, BehaviorsStr}  = file:read_file("config/behaviors.json"),
    %    %ActionsData         = jiffy:decode(ActionsStr),
    %    %BehaviorsData       = jiffy:decode(BehaviorsStr),
    %    ok
    %catch
    %    Exception:Reason -> {error, Exception, Reason}
    %end.
