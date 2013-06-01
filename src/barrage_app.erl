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
    case loadConfigTable() of
        ok ->
            [{_, Port}] = ets:lookup(barrage, admin_port),
            [{_, General}] = ets:lookup(barrage, enable_general),
            [{_, Commander}] = ets:lookup(barrage, enable_commander),
            case {General, Commander} of
                {true, true} ->
                    Routes = barrage_general_handler:routes() ++
                             barrage_commander_handler:routes() ++
                             barrage_general_ws_handler:routes() ++
                             [{"/[...]", cowboy_static, [
                                    {directory, {priv_dir, barrage, []}},
                                    {mimetypes, 
                                        {fun mimetypes:path_to_mimes/2, default}}
                                ]}], 
                    Dispatch = cowboy_router:compile([{'_', Routes } ]),
                    %% this is a general so load up the correct end points
                    {ok, _} = cowboy:start_http(http, 100, [{port, Port}], [
                            {env, [{dispatch, Dispatch}]}
                    ]);
                {true, false} ->
                    Routes = barrage_general_handler:routes() ++
                             barrage_general_ws_handler:routes() ++
                             [{"/[...]", cowboy_static, [
                                    {directory, {priv_dir, barrage, []}},
                                    {mimetypes, 
                                        {fun mimetypes:path_to_mimes/2, default}}
                                ]}], 
                    Dispatch = cowboy_router:compile([{'_', Routes } ]),
                    %% this is a general so load up the correct end points
                    {ok, _} = cowboy:start_http(http, 100, [{port, Port}], [
                            {env, [{dispatch, Dispatch}]}
                    ]);
                {false, true} ->
                    Routes = barrage_commander_handler:routes() ++
                             [{"/[...]", cowboy_static, [
                                    {directory, {priv_dir, barrage, []}},
                                    {mimetypes, 
                                        {fun mimetypes:path_to_mimes/2, default}}
                                ]}], 
                    Dispatch = cowboy_router:compile([{'_', Routes } ]),
                    %% this is a general so load up the correct end points
                    {ok, _} = cowboy:start_http(http, 100, [{port, Port}], [
                            {env, [{dispatch, Dispatch}]}
                    ])
            end,
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
get_config_file(undefined) ->
    "/barrage.json";
get_config_file(Value) ->
    binary_to_list(Value).

get_behavior_file(undefined) ->
    "/behaviors.json";
get_behavior_file(Value) ->
    binary_to_list(Value).

get_action_file(undefined) ->
    "/behaviors.json";
get_action_file(Value) ->
    binary_to_list(Value).

get_file_names() ->
    BasePath        = code:priv_dir(barrage),
    case filelib:is_regular(BasePath ++ "/files.json") of
        true ->
            {ok, FilesJ}= file:read_file(BasePath ++ "/files.json"), 
            {Files}     = jiffy:decode(FilesJ),
            {
                BasePath ++ 
                get_config_file(proplists:get_value(<<"config">>, Files)),
                BasePath ++ 
                get_behavior_file(proplists:get_value(<<"behaviors">>, Files)),
                BasePath ++ 
                get_action_file(proplists:get_value(<<"actions">>, Files))
            };
        false ->
            {   BasePath ++ "/barrage.json", 
                BasePath ++ "/behaviors.json", 
                BasePath ++ "/actions.json"}
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
    try
        {BarrageConfig, BehaviorConfig, ActionConfig} = get_file_names(),
        {ok, ConfigsJ}  = file:read_file(BarrageConfig),
        {ok, PlansJ}    = file:read_file(BehaviorConfig),
        {ok, ActionsJ}  = file:read_file(ActionConfig),
        
        Configs         = jiffy:decode(ConfigsJ),
        Plans           = jiffy:decode(PlansJ),
        Actions         = jiffy:decode(ActionsJ),
    
        % Now lets set parse the plans in order to build out an ets table
        ok = barrage_parser:load_sys_data(Configs),
        ok = barrage_parser:load_behavior_data(Plans),
        ok = barrage_parser:load_action_data(Actions)
    catch 
        _:_ ->
            error_data
    end.

