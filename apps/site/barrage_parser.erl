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
%%% Created : 2013-04-19 13:23:18.998358
%%%-------------------------------------------------------------------
-module(barrage_parser).

%% API
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

