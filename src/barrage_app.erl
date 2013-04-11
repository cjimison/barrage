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
-export([process_json_action/1]).

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
    inets:start(),
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
process_actions_config([]) ->
    ok;
process_actions_config(Actions) ->
    [{Action} | OtherActions]   = Actions,
    Name                        = proplists:get_value(name, Action),
    ets:insert(actions, {Name, Action}),
    process_actions_config(OtherActions).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
process_actions_json([]) ->
    ok;
process_actions_json(Actions) ->
    [{JSONAction}|OtherActions] = Actions,
    Name                        = proplists:get_value(<<"name">>, JSONAction),
    Action = process_json_action(JSONAction),
    ets:insert(actions, {Name, Action}),
    process_actions_config(OtherActions).


%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
build_action_list(Name, URL, Type) when Name    /= undefined,
                                        URL     /= undefined,
                                        Type    /= undefined ->
    [{name, Name}, {url, URL}, {type, binary_to_atom(Type, utf8)}];
build_action_list(_, _, _) ->
    error.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
build_args_list(<<"http">>, Args) when Args /= undefined ->
    {PArgs} = Args,
    [{args_type, http}, {args,PArgs}];
build_args_list(<<"json">>, Args) when Args /= undefined ->
    {PArgs} = Args,
    [{args_type, json}, {args,PArgs}];
build_args_list(_, _) ->
    error.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
build_results_list(<<"json">>, Results) when Results /= undefined ->
    {PRez} = Results,
    [{results_type, json}, {args,PRez}];
build_results_list(<<"http">>, Results) when Results /= undefined ->
    {PRez} = Results,
    [{results_type, http}, {args,PRez}];
build_results_list(_, _) ->
    error.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
build_action(error, _ArgsList, _ResultsList) ->
    error;
build_action(Action, error, error) ->
    Action;
build_action(Action, ArgsList, error) ->
    Action ++ ArgsList;
build_action(Action, error, ResultsList) ->
    Action ++ ResultsList;
build_action(Action, ArgsList, ResultsList) ->
    Action ++ ArgsList ++ ResultsList.


%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
process_json_action(JSONAction) ->
    Name        = proplists:get_value(<<"name">>, JSONAction),
    URL         = proplists:get_value(<<"url">>, JSONAction),
    Type        = proplists:get_value(<<"type">>, JSONAction),
    ActionList  = build_action_list(Name, URL, Type),
    
    case ActionList of
        error ->
            error;
        _ ->
            %% Optional params
            Args        = proplists:get_value(<<"args">>, JSONAction),
            ArgsType    = proplists:get_value(<<"args_type">>, JSONAction),
            ArgsList    = build_args_list(ArgsType, Args), 
            Results     = proplists:get_value(<<"results">>, JSONAction),
            ResType     = proplists:get_value(<<"results_type">>, JSONAction),
            ResList     = build_results_list(ResType, Results),
            build_action(ActionList, ArgsList, ResList)
    end. 

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
load_system_files() ->
    BasePath        = code:priv_dir(barrage),
    {Rez, Configs}  = file:consult(BasePath ++ "/barrage.config"), 
    case Rez of 
        ok ->
            process_config(Configs);
        _ ->
            error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
load_action_files() ->
    BasePath        = code:priv_dir(barrage),
    {ARez, Actions} = file:consult(BasePath ++ "/actions.config"),
    case ARez of ok ->
        process_actions_config(Actions)
    end,

    {ARez2, Actions2} = file:read_file(BasePath ++ "/actions.json"),
    case ARez2 of ok ->
            process_actions_json(jiffy:decode(Actions2))
    end.

process_behavior_json([]) ->
    ok;
process_behavior_json(Behaviors) ->
    [{Behavior} | OtherBehaviors]   = Behaviors,
    Name                            =  proplists:get_value(<<"name">>, Behavior),
    Tree                            =  proplists:get_value(<<"tree">>, Behavior),

    process_behavior_json(OtherBehaviors).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
load_behavior_files() ->

    BasePath        = code:priv_dir(barrage),
    {BRez, Behavior}= file:read_file(BasePath ++ "/behaviors.config"),
    case BRez of ok ->
            process_behavior_json(jiffy:decode(Behavior))
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%%  Loads all the config files that currently exist for the system
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
loadConfigTable()->
    ets:new(barrage,    [set, named_table]),
    ets:new(actions,    [set, named_table]),
    ets:new(plans,      [set, named_table]),

    ok = load_system_files(),
    ok = load_action_files(),
    ok = load_behavior_files().

