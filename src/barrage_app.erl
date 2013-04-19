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
                    {error, Error}
            end;
        Error ->
            {error, Error}
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

process_plans(Plans) when true =:= is_list(Plans) ->
    [ThePlan | OtherPlans]  = Plans,
    case is_tuple(ThePlan) of
        true ->
            {Plan}  = ThePlan,
            Name    = proplists:get_value(<<"name">>, Plan),
            Tree    = proplists:get_value(<<"tree">>, Plan),
            
            case (Name /= undefined andalso Tree /= undefined) of
                true ->
                    [{table_keys, OldKeys}] = ets:lookup(plans, table_keys),
                    NewKeys = [Name | OldKeys],
                    ets:insert(plans, {table_keys, NewKeys}),
                    ets:insert(plans, {Name, Tree}),
                    process_plans(OtherPlans);
                false ->
                    invalid_plan_elements
            end;
        false ->
            invalid_plan
    end;

process_plans(_Plans) ->
    invalid_plans.

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

process_actions(Actions) when true =:= is_list(Actions) ->
    [TheAction | OtherActions]  = Actions,
    case is_tuple(TheAction) of
        true ->
            {Action}= TheAction,
            Name    = proplists:get_value(<<"name">>, Action),
            case (Name /= undefined) of
                true ->
                    ets:insert(actions, {Name, Action}),
                    process_actions(OtherActions);
                false ->
                    invalid_action_element
            end;
        false ->
            invalid_action
    end;

process_actions(_Actions) ->
    invalid_actions.

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

process_config(Configs) when true =:= is_list(Configs) ->
    [Config | OtherConfigs] = Configs,
    case process_config(Config) of
        ok ->
            process_config(OtherConfigs);
        Error ->
            Error
    end;

process_config(TheConfig) when true =:= is_tuple(TheConfig) ->
    {Config} = TheConfig,
    Type     = proplists:get_value(<<"type">>, Config),
    TheArgs  = proplists:get_value(<<"args">>, Config),
    case (undefined /= Type andalso undefined /= TheArgs andalso is_tuple(TheArgs)) of
        true ->
            {Args} = TheArgs,
            ets:insert(barrage, {type, Type}),
            case Type of
                <<"general">> ->
                    ets:insert(barrage, {enable_general, true}),
                    case proplists:get_value(<<"url">>, Args) of
                        undefined ->
                            invalid_config_arg;
                        URL ->
                            ets:insert(barrage, {url, URL}),
                            ok
                    end;
                <<"commander">> ->
                    ets:insert(barrage, {enable_commander, true}),
                    Gunners = proplists:get_value(<<"gunners">>, Args),
                    GeneralA= proplists:get_value(<<"general">>, Args),
                    case (Gunners /= undefined andalso GeneralA /= undefined) of
                        true ->
                            General = binary_to_atom(GeneralA, utf8),
                            ets:insert(barrage, {general, General}),
                            ets:insert(barrage, {gunners, Gunners}),
                            ok;
                        false ->
                            invalid_config_arg
                    end;
                _ ->
                    invalid_config_type

            end;
        false ->
            invalid_config
    end;

process_config(_Configs) ->
    invalid_config.

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
    try

        {ok, ConfigsJ}  = file:read_file(BasePath ++ "/barrage.json"),
        {ok, PlansJ}    = file:read_file(BasePath ++ "/behaviors.json"),
        {ok, ActionsJ}  = file:read_file(BasePath ++ "/actions.json"),
        
        Configs         = jiffy:decode(ConfigsJ),
        Plans           = jiffy:decode(PlansJ),
        Actions         = jiffy:decode(ActionsJ),
        
        % Now lets set parse the plans in order to build out an ets table
        ets:new(barrage, [set, named_table]),

        ets:insert(barrage, {enable_general, false}),
        ets:insert(barrage, {enable_commander, false}),
        ok = process_config(Configs),

        ets:new(plans, [set, named_table]),
        ets:insert(plans, {table_keys, []}),
        ok = process_plans(Plans),

        ets:new(actions, [set, named_table]),
        process_actions(Actions)

    catch 
        _:_ ->
            error_data
    end.

