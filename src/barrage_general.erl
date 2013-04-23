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
-module(barrage_general).

-behaviour(gen_server).

%% App layer API
-export([start_link/0]).            %<<- Starts up the server
-export([issue_order/1]).           %<<- Tells the commanders to attack
-export([issue_http_order/2]).      %<<- Tells the commanders to attack
-export([report_results/2]).        %<<- callback by commander when done
-export([enlist/1]).                %<<- Adds a commander to the pool 
-export([retire/1]).                %<<- Removes a commander from the pool
-export([get_commanders_info/0]).   %<<- Get info about all commanders

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
        commanders=[],
        blocked = [],
        waiting_pid = [],
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
    {ok, #state{}}.

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

get_commanders_info() ->
    gen_server:call(?MODULE, {get_commanders_info}).


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
                    NewState = State#state{ results=dict:new(), 
                                            blocked=State#state.commanders},
                    [{_, TargetServer}] = ets:lookup(barrage, server),
                    [{_, TargetPort}] = ets:lookup(barrage, port),
                    Fun = fun(Pid) ->
                            barrage_commander:execute(Pid, Order, TargetServer, TargetPort) end,
                    lists:foreach(Fun, State#state.commanders),
                    {reply, ok, NewState};
                _->
                    %No test is going to be run.
                    {reply, ok, State}
            end
    end;

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
    Commanders = [PID | State#state.commanders],
    NewState = State#state{commanders=Commanders}, 
    {reply, ok, NewState};

%%%%------------------------------------------------------------------
%%%% retire
%%%%------------------------------------------------------------------
handle_call({retire, PID}, _From, State) ->
    Commanders = State#state.commanders -- [PID],
    NewState = State#state{commanders=Commanders}, 
    {reply, ok, NewState};

%%%%------------------------------------------------------------------
%%%% report_results
%%%%------------------------------------------------------------------
handle_call({report_results, CPID, Results}, _From, State) ->
    Result              = process_results(  Results, 
                                            State#state.results),
    Commanders          = State#state.blocked,
    ActiveCommanders    = lists:delete(CPID, Commanders),
    case ActiveCommanders of 
        [] ->
            Fun = fun(Pid) -> Pid ! {done, Result} end,
            lists:foreach(Fun, State#state.waiting_pid),
            {reply, ok, State#state{waiting_pid = [], blocked=[], results=dict:new()}};
        _ ->
            {reply, ok, State#state{blocked = ActiveCommanders, results=Result}}
    end;

%%%%------------------------------------------------------------------
%%%% get_commanders_info
%%%%------------------------------------------------------------------
handle_call({get_commanders_info}, _From, State) ->
    Commanders = State#state.commanders,
    CommanderInfo = get_commanders_names(Commanders, []),
    {reply, CommanderInfo, State};

%%%%------------------------------------------------------------------
%%%% UNKNOWN COMMAND
%%%%------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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

get_commanders_names([], CommanderData) ->
    CommanderData;

get_commanders_names(Commanders, CommanderData) ->
    [CommanderPid | OtherCommanders] = Commanders,
    CommanderName = atom_to_binary(node(CommanderPid), utf8),
    get_commanders_names(OtherCommanders, [CommanderName | CommanderData]).

process_results([], MergedDict) ->
    MergedDict;

process_results(Results, MergedDict) ->
    [Result | OtherResults] = Results,
    Fun = fun(_Key, Value1, Value2) -> Value1 ++ Value2 end,
    NewDict = dict:merge(Fun, MergedDict, Result),
    process_results(OtherResults, NewDict).

