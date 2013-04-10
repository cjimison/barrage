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
%%% @doc
%%%
%%% @end
%%% Created : 2013-04-03 14:34:43.871327
%%%-------------------------------------------------------------------
-module(barrage_general).

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([test_run/0,
         issue_order/1,
         report_results/2,
         enlist/1,
         retire/1]).

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
        commanders=[]
    }).

%%%===================================================================
%%% API
%%%===================================================================

test_run()->
    % Now issue an order out to the commanders for execution
    barrage_general:issue_order(<<"Random Commands">>).

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

report_results(CommanderPid, Results)->
    gen_server:call(?MODULE, {report_results, CommanderPid, Results}).

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
handle_call({issue_order, OrderName}, _From, State) ->
    [{_, Order}] = ets:lookup(plans, OrderName), 
    [{_, TargetIP}] = ets:lookup(barrage, url),
    Fun = fun(Pid) ->
            barrage_commander:execute(Pid, Order, TargetIP) end,
    lists:foreach(Fun, State#state.commanders),
    {reply, ok, State};

handle_call({enlist, PID}, _From, State) ->
    Commanders = [PID | State#state.commanders],
    NewState = State#state{commanders=Commanders}, 
    {reply, ok, NewState};

handle_call({retire, PID}, _From, State) ->
    Commanders = State#state.commanders -- [PID],
    NewState = State#state{commanders=Commanders}, 
    {reply, ok, NewState};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages for report_results
%%
%% @spec handle_call({report_results, CPID, Results}, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%%  CPID is the commander Pid
%%  Results = Array of reports
%%            Report = Dictionary of Key values.  Key is the action name.  Value is array of times 
%% @end
%%--------------------------------------------------------------------
handle_call({report_results, _CPID, Results}, _From, State) ->
    Result = process_results(Results, dict:new()),
    Keys = dict:fetch_keys(Result),
    display_result(Keys, Result),
    %Keys = dict:fetch_keys(Results),
    %process_results(Results, Keys),
    {reply, ok, State};


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
display_result([], _Result)->
    ok;
display_result(Keys, Result)->
    [Key | OtherKeys]   = Keys,
    {ok, Data}          = dict:find(Key, Result),
    SortData            = lists:sort(Data),
    Size                = length(Data),
    Average             = lists:sum(Data) / Size,
    High                = lists:last(SortData),
    [Low | _]           = SortData,
    io:format("~n~nAction: ~p --------~n", [Key]),
    io:format("Number of requests =: ~p~n", [Size]),
    io:format("Average Time(ms)   =: ~p~n", [Average/1000]),
    io:format("High Time(ms)      =: ~p~n", [High/1000]),
    io:format("Low Time(ms)       =: ~p~n", [Low/1000]),

    display_result(OtherKeys, Result).

process_results([], MergedDict) ->
    MergedDict;

process_results(Results, MergedDict) ->
    [Result | OtherResults] = Results,
    Fun = fun(_Key, Value1, Value2) -> Value1 ++ Value2 end,
    NewDict = dict:merge(Fun, MergedDict, Result),
    process_results(OtherResults, NewDict).

