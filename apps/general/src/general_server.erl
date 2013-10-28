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
%%% Created : 2013-04-03 14:34:43.871327
%%%-------------------------------------------------------------------
-module(general_server).

-behaviour(gen_server).

%% App layer API
-export([start_link/0]).            %<<- Starts up the server
-export([issue_order/1]).           %<<- Tells the commanders to attack
-export([issue_http_order/2]).      %<<- Tells the commanders to attack
-export([report_results/2]).        %<<- callback by commander when done
-export([stream_results/1]).        %<<- A streamed log coming in from a commander
-export([enlist/1]).                %<<- Adds a commander to the pool 
-export([retire/1]).                %<<- Removes a commander from the pool
-export([get_commanders_info/0]).   %<<- Get info about all commanders
-export([change_cookie/1]).

-export([reg_stream/1]).
-export([unreg_stream/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state,
    {
        blocked         = [],
        waiting_pid     = [],
        commanders      = dict:new(),
        streams         = [],
        results
    }).

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
start_link() ->
        gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    ActFile         = code:priv_dir(site) ++ "/actions.json",
    BehFile         = code:priv_dir(site) ++ "/behaviors.json",
    ActTemplate     = code:priv_dir(site) ++ "/actionsTemplate.json",
    BehTemplate     = code:priv_dir(site) ++ "/behaviorsTemplate.json",
    
    {ok, ActJSON}   = file:read_file(ActFile),
    {ok, BehJSON}   = file:read_file(BehFile),
    
    {ok,ActTempJSON}= file:read_file(ActTemplate),
    {ok,BehTempJSON}= file:read_file(BehTemplate),

    Actions         = jiffy:decode(ActJSON),
    Behaviors       = jiffy:decode(BehJSON),

    application:set_env(general, actions, Actions),
    application:set_env(general, behaviors, Behaviors),

    application:set_env(general, actions_template,  ActTempJSON),
    application:set_env(general, behavior_template, BehTempJSON),

    load_action_data(Actions),
    load_behavior_data(Behaviors),

    {ok, #state{}}.

reg_stream(StreamID) ->
    gen_server:call(?MODULE, {reg_stream, StreamID}).

unreg_stream(StreamID) ->
    gen_server:call(?MODULE, {unreg_stream, StreamID}).

enlist(CommanderPid) ->
    gen_server:call(?MODULE, {enlist, CommanderPid}).

retire(CommanderPid) ->
    gen_server:call(?MODULE, {retire, CommanderPid}).

issue_order(Order) ->
    gen_server:call(?MODULE, {issue_order, Order}).

issue_http_order(Order, Pid) ->
    gen_server:call(?MODULE, {issue_http_order, Order, Pid}).

report_results(CommanderPid, Results)->
    gen_server:call(?MODULE, {report_results, CommanderPid, Results}).

stream_results(Results)->
    gen_server:call(?MODULE, {stream_results, Results}).

get_commanders_info() ->
    gen_server:call(?MODULE, {get_commanders_info}).

change_cookie(Cookie) ->
    gen_server:call(?MODULE, {change_cookie, Cookie}).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

%%%%------------------------------------------------------------------
%%%% issue_order
%%%%------------------------------------------------------------------
handle_call({issue_order, OrderName}, _From, State) ->
    [{_, Order}] = ets:lookup(plans, OrderName),
    case Order of 
        [] ->
            {reply, error_no_match, State};
        _ ->
            case State#state.blocked of
                [] ->
                    Pids = dict:fetch_keys(State#state.commanders),
                    NewState = State#state{ results=dict:new(), 
                                            blocked=Pids},
                    {ok, TargetServer}  = application:get_env(general, target_ip),
                    {ok, TargetPort}    = application:get_env(general, target_port),
                    Plans               = ets:tab2list(plans),
                    Actions             = ets:tab2list(actions),
                    
                    SetFun              = fun(Pid) ->
                            commander_server:set_data(Pid, Plans, Actions)
                    end,

                    Fun                 = fun(Pid) ->
                            commander_server:execute(Pid, Order, 
                                                     TargetServer, TargetPort)
                    end,
                    lists:foreach(SetFun, Pids),
                    lists:foreach(Fun, Pids),
                    {reply, ok, NewState};
                _->
                    %No test is going to be run.
                    {reply, ok, State}
            end
    end;

%%%%------------------------------------------------------------------
%%%% reg_stream
%%%%------------------------------------------------------------------
handle_call({reg_stream, Pid}, _From, State) ->
    NewState = State#state{streams= State#state.streams ++ [Pid]},
    {reply, ok, NewState};

%%%%------------------------------------------------------------------
%%%% unreg_stream
%%%%------------------------------------------------------------------
handle_call({unreg_stream, Pid}, _From, State) ->
    Streams = lists:delete(Pid, State#state.streams),
    NewState= State#state{streams=Streams},
    {reply, ok, NewState};

%%%%------------------------------------------------------------------
%%%% issue_http_order
%%%%------------------------------------------------------------------
handle_call({issue_http_order, OrderName, Pid}, _From, State) ->
    Pids = State#state.waiting_pid,
    NewState = State#state{waiting_pid = [Pid| Pids]},
    handle_call({issue_order, OrderName}, _From, NewState);

%%%%------------------------------------------------------------------
%%%% enlist
%%%%------------------------------------------------------------------
handle_call({enlist, PID}, _From, State) ->
    Ref = erlang:monitor(process, PID),
    PidDict = dict:store(PID, Ref, State#state.commanders),
    NewState = State#state{commanders = PidDict}, 
    {reply, {ok, self()}, NewState};

%%%%------------------------------------------------------------------
%%%% retire
%%%%------------------------------------------------------------------
handle_call({retire, PID}, _From, State) ->
    {ok, Ref}   = dict:find(PID, State#state.commanders),
    NewDict     = dict:erase(PID, State#state.commanders), 
    NewState    = State#state{commanders = NewDict}, 
    erlang:demonitor(Ref),
    {reply, ok, NewState};

%%%%------------------------------------------------------------------
%%%% report_results
%%%%------------------------------------------------------------------
handle_call({report_results, CPID, Results}, _From, State) ->
    SendFunc = fun(Pid) ->
            handler_ws_general:send(Pid, {[{cmd,exit}]})
    end,
    lists:foreach(SendFunc, State#state.streams),

    Result              = process_results(  Results, 
                                            State#state.results),
    Commanders          = State#state.blocked,
    ActiveCommanders    = lists:delete(CPID, Commanders),
    case ActiveCommanders of 
        [] ->
            Fun = fun(Pid) -> Pid ! {done, Result} end,
            lists:foreach(Fun, State#state.waiting_pid),
            {reply, ok, State#state{
                                        waiting_pid = [],
                                        blocked     = [],
                                        streams     = [],
                                        results     = dict:new()
                                    }};
        _ ->
            {reply, ok, State#state{
                                        blocked     = ActiveCommanders,
                                        streams     = [],
                                        results     = Result}}
    end;

handle_call({stream_results, Results}, _From, State) ->
    SendFunc = fun(Pid) ->
        handler_ws_general:send(Pid, Results)
    end,
    lists:foreach(SendFunc, State#state.streams),
    {reply, ok, State};

%%%%------------------------------------------------------------------
%%%% get_commanders_info
%%%%------------------------------------------------------------------
handle_call({get_commanders_info}, _From, State) ->
    Commanders = dict:fetch_keys(State#state.commanders),
    CommanderInfo = get_commanders_info(Commanders, []),
    {reply, CommanderInfo, State};


handle_call({change_cookie, Cookie}, _From, State) when
    State#state.blocked == [] ->
    DisFun = fun(Pid) -> 
        commander_server:disconnect(Pid) 
    end,
    lists:foreach(DisFun, dict:fetch_keys(State#state.commanders)),
    true = erlang:set_cookie(node(), Cookie),
    {reply, ok, State};

%%%%------------------------------------------------------------------
%%%% UNKNOWN COMMAND
%%%%------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
        {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'DOWN', _Ref, process, CPid, _Reason}, State) ->
    NewDict             = dict:erase(CPid, State#state.commanders), 
    Commanders          = State#state.blocked,
    ActiveCommanders    = lists:delete(CPid, Commanders),
    case ActiveCommanders of 
        [] ->
            {noreply, State#state{
                                    waiting_pid = [],
                                    blocked     = [], 
                                    results     = dict:new(),
                                    commanders   = NewDict
                                }};
        _ ->
            {noreply, State#state{ 
                                    blocked     = ActiveCommanders,
                                    commanders  = NewDict
                                }}
    end;

handle_info(_Info, State) ->
        {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
        ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_commanders_info([], CommanderData) ->
    CommanderData;

get_commanders_info(Commanders, CommanderData) ->
    [CommanderPid | OtherCommanders] = Commanders,
    CommanderName = atom_to_binary(node(CommanderPid), utf8),
    Count = commander_server:gunner_count(CommanderPid),
    get_commanders_info(OtherCommanders, 
        [{[{<<"name">>, CommanderName}, {<<"count">>, Count}]} | CommanderData]).

list_merge([], [], Rez)->
    Rez;

list_merge(L1, [], Rez)->
    Rez ++ L1;

list_merge([], L2, Rez)->
    Rez ++ L2;

list_merge(L1, L2, Rez)->
    [L1A | L1Others] = L1,
    [L2A | L2Others] = L2,
    NewRez = Rez ++ [L1A, L2A],
    list_merge(L1Others, L2Others, NewRez).

process_results([], MergedDict) ->
    MergedDict;

process_results(Results, MergedDict) ->
    [Result | OtherResults] = Results,
    Fun = fun(_Key, Value1, Value2) -> 
            list_merge(Value1, Value2, [])
    end,
    NewDict = dict:merge(Fun, MergedDict, Result),
    process_results(OtherResults, NewDict).

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


