%%%-------------------------------------------------------------------
%%% @author Chris Jimison
%%% @copyright (C) 2013, Not Rigged Games LLC
%%% @doc
%%%
%%% @end
%%% Created : 2013-04-19 13:23:18.998358
%%%-------------------------------------------------------------------
-module(barrage_parser).

%% API
-export([load_sys_data/1]).
-export([load_behavior_data/1]).
-export([load_action_data/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% 
%% @spec 
%% @end
%%--------------------------------------------------------------------
load_sys_data(Configs) ->
    TableInfo = ets:info(barrage),
    case TableInfo of 
        undefined ->
            ets:new(barrage, [set, named_table, public]);
        _ ->
            ok
    end,
    ets:insert(barrage, {enable_general, false}),
    ets:insert(barrage, {enable_commander, false}),
    process_sys_data(Configs).
    
%%--------------------------------------------------------------------
%% @doc
%% 
%% @spec 
%% @end
%%--------------------------------------------------------------------
load_behavior_data(Behaviors) ->
    TableInfo = ets:info(plans),
    case TableInfo of 
        undefined ->
            ets:new(plans, [set, named_table, public]),
            ets:insert(plans, {table_keys, []});
        _ ->
            ets:insert(plans, {table_keys, []}),
            ok
    end,
    process_behavior_data(Behaviors).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @spec 
%% @end
%%--------------------------------------------------------------------
load_action_data(Actions) ->
    TableInfo = ets:info(actions),
    case TableInfo of 
        undefined ->
            ets:new(actions, [set, named_table, public]);
        _ ->
            ok
    end,
    process_action_data(Actions).


%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @spec process_behavior_data(Plans) -> ok
%%      Plans = Array
%% @end
%%--------------------------------------------------------------------
process_behavior_data([]) ->
    ok;

process_behavior_data(Plans) when true =:= is_list(Plans) ->
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
                    process_behavior_data(OtherPlans);
                false ->
                    invalid_plan_elements
            end;
        false ->
            invalid_plan
    end;

process_behavior_data(_Plans) ->
    invalid_plans.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%  Process the Config data
%%
%% @spec process_actions(Actions) -> ok
%%      Plans = Array
%% @end
%%--------------------------------------------------------------------
process_sys_data([]) ->
    ok;

process_sys_data(Configs) when true =:= is_list(Configs) ->
    [Config | OtherConfigs] = Configs,
    case process_sys_data(Config) of
        ok ->
            process_sys_data(OtherConfigs);
        Error ->
            Error
    end;

process_sys_data(TheConfig) when true =:= is_tuple(TheConfig) ->
    {Config} = TheConfig,
    Type     = proplists:get_value(<<"type">>, Config),
    ets:insert(barrage, {type, Type}),
    Args     = get_args_block(Config),
    
    case Type of
        <<"general">> ->
            ets:insert(barrage, {enable_general, true}),
            Server  = get_server_ip(Args),
            Port    = get_port_val(Args),
            ets:insert(barrage, {server, Server}),
            ets:insert(barrage, {port, Port}),
            ok;
        <<"commander">> ->
            ets:insert(barrage, {enable_commander, true}),
            Gunners = get_gunner_count(Args),
            General = get_general_name(Args),
            Connect = get_connected_val(Args),
            ets:insert(barrage, {general, General}),
            ets:insert(barrage, {gunners, Gunners}),
            ets:insert(barrage, {connect_on_launch, Connect}),
            %% If the port is already set by the general then
            %% just use it.  It is an override
            case ets:lookup(barrage, port) of
                [] ->
                    ets:insert(barrage, {port, get_port_val(Args)});
                _ ->
                    ok
            end,
            ok;
        _ ->
            invalid_config_type
    end;

process_sys_data(_Configs) ->
    invalid_config.

get_server_ip(Args) ->
    case proplists:get_value(<<"server">>, Args) of
        undefined ->
            <<"127.0.0.1">>;
        Server ->
            Server
    end.

get_args_block(Config) ->
    case proplists:get_value(<<"args">>, Config) of 
        undefined ->
            [];
        {Args} ->
            Args
    end.

get_port_val(Args) ->
    case proplists:get_value(<<"port">>, Args) of
        undefined ->
            8080;
        Port ->
            Port
    end.

get_connected_val(Args) ->
    General = proplists:get_value(<<"general">>, Args),
    Connect = proplists:get_value(<<"connect_on_launch">>, Args),
    case {General, Connect} of
        {undefined, _ } ->
            false;
        {_, undefined } ->
            false;
        {_, _} ->
            Connect
    end.

get_general_name(Args) ->
    case proplists:get_value(<<"general">>, Args) of
        undefined ->
            not_set;
        General ->
            binary_to_atom(General, utf8)
    end.

get_gunner_count(Args) ->
    case proplists:get_value(<<"gunners">>, Args) of
        undefined ->
            10;
        Count ->
            Count
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%  Process the Actions defined by the user
%%
%% @spec process_actions(Actions) -> ok
%%      Plans = Array
%% @end
%%--------------------------------------------------------------------
process_action_data([]) ->
    ok;

process_action_data(Actions) when true =:= is_list(Actions) ->
    [TheAction | OtherActions]  = Actions,
    case is_tuple(TheAction) of
        true ->
            {Action}= TheAction,
            Name    = proplists:get_value(<<"name">>, Action),
            case (Name /= undefined) of
                true ->
                    ets:insert(actions, {Name, Action}),
                    process_action_data(OtherActions);
                false ->
                    invalid_action_element
            end;
        false ->
            invalid_action
    end;

process_action_data(_Actions) ->
    invalid_actions.

