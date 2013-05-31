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
%%% Created : 2013-04-03 14:34:25.679330
%%%-------------------------------------------------------------------
-module(barrage_commander).

-behaviour(gen_server).

%% API
-export([start/0, 
         start_link/0,
         set_data/3,
         execute/4,
         gunner_count/0,
         gunner_count/1,
         change_cookie/1,
         change_general/1,
         connect/0,
         disconnect/0,
         disconnect/1,
         is_connected/0,
         change_gunner_count/1,
         order_complete/2,
         log_results/3
     ]).

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
        general             = null,
        gunners             = [],
        executing           = false,
        wait_count          = 0,
        connected           = false,
        reports             = [],
        streaming_reports   = []
    }).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
        gen_server:start(?MODULE, [], []).

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
gunner_count() ->
    gen_server:call(?MODULE, {gunner_count}).

gunner_count(Pid) ->
    gen_server:call(Pid, {gunner_count}).

set_data(Pid, Plans, Actions) ->
    gen_server:cast(Pid, {set_data, {Plans, Actions}}).

execute(Pid, Orders, Server, Port) ->
    gen_server:cast(Pid, {execute, {Orders, Server, Port}}).

order_complete(GunnerPid, Results) ->
    gen_server:cast(?MODULE, {orders_complete, GunnerPid, Results}).

change_cookie(Cookie) ->
    gen_server:call(?MODULE, {change_cookie, Cookie}).

change_general(General) ->
    gen_server:call(?MODULE, {change_general, General}).

change_gunner_count(Count) ->
    gen_server:call(?MODULE, {change_gunners, Count}).

disconnect() ->
    gen_server:call(?MODULE, disconnect).

disconnect(Pid) ->
    gen_server:call(Pid, disconnect).

connect() ->
    gen_server:call(?MODULE, connect).

is_connected() ->
    gen_server:call(?MODULE, is_connected).

log_results(GunnerPID, ActionName, Time) ->
    gen_server:cast(?MODULE, {log_results, GunnerPID, ActionName, Time}).

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
    [{_, Connect}] = ets:lookup(barrage, connect_on_launch),
    [{_, General}] = ets:lookup(barrage, general),
    [{_, GunnerCount}] = ets:lookup(barrage, gunners),
    case Connect of
        true ->
            State = #state{ general     = General, 
                            gunners     = create_gunners([], GunnerCount),
                            connected   = true
                        },
            rpc:call(General, barrage_general, enlist, [self()]),
            {ok, State};
        _ ->
            State = #state{ general     = General, 
                            gunners     = create_gunners([], GunnerCount),
                            connected   = false
                        },
            {ok, State}
    end.


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
handle_call({gunner_count}, _From, State) -> 
    Count = length(State#state.gunners),
    {reply, Count, State};

handle_call({change_cookie, Cookie}, _From, State) when 
        State#state.executing == false ->
    true = erlang:set_cookie(node(), Cookie),
    {reply, ok, State};

handle_call({change_general, General}, _From, State) when 
        State#state.executing == false ->
    %[{_, OldGeneral}] = ets:lookup(barrage, general),
    %rpc:call(OldGeneral, barrage_general, retire, [self()]),
    ets:insert(barrage, {general, General}),
    %rpc:call(General, barrage_general, enlist, [self()]),
    {reply, ok, State};

handle_call({change_gunners, Count}, _From, State) when 
        State#state.executing == false ->
    destroy_gunners(State#state.gunners),
    NewState = #state{gunners = create_gunners([], Count)},
    {reply, ok, NewState};


handle_call(connect, _From, State) when 
        State#state.executing == false ->
    [{_, General}] = ets:lookup(barrage, general),
    rpc:call(General, barrage_general, enlist, [self()]),
    NewState = State#state{connected=true},
    {reply, ok, NewState};

handle_call(disconnect, _From, State) when 
        State#state.executing == false ->
    [{_, General}] = ets:lookup(barrage, general),
    rpc:call(General, barrage_general, retire, [self()]),
    NewState = State#state{connected=false},
    {reply, ok, NewState};

handle_call(is_connected, _From, State) ->
    {reply, State#state.connected, State};

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
handle_cast({set_data, {Plans, Actions}}, State) when 
    State#state.executing == false ->
    true = ets:insert(plans, Plans),
    true = ets:insert(actions, Actions),
    {noreply, State};

handle_cast({execute, {Order, Server, Port}}, State) when 
        State#state.executing == false ->
    %NOTE: I am currently doing this in two phases
    %      so the timing syncs up a little better.
    %      I may move this to a single phase step.
    %      REFACTOR: Have the follow_order take a
    %      target URL as well.
    FunTarget = fun(Pid) -> 
            barrage_gunner:set_server_info(Pid, Server, Port)
    end,

    FunFire = fun(Pid) ->
            barrage_gunner:follow_order(Pid, Order) 
    end,

    %Have all the gunners sight in the target
    lists:foreach(FunTarget, State#state.gunners),

    NewState = State#state{ executing   = true, 
                            wait_count  = length(State#state.gunners),
                            reports     = []
                          }, 
    %The open fire!!!
    lists:foreach(FunFire, State#state.gunners),
    {noreply, NewState};

handle_cast({execute, {_Order, _TargetURL}}, State) when 
        State#state.executing == true ->
    {noreply, State};

handle_cast({orders_complete, _GunnerPid, Results}, State) when 
        State#state.wait_count == 1 ->
    rpc:call(   State#state.general, 
                barrage_general, 
                report_results, 
                [self(), [Results | State#state.reports]]),
    NewState    = State#state{ executing = false },
    {noreply, NewState};

handle_cast({orders_complete, _GunnerPid, Results}, State) ->
    NewState = State#state{
                            wait_count = State#state.wait_count -1,
                            reports = [Results | State#state.reports]
                          }, 
    {noreply, NewState};

handle_cast({log_results, ActionName, Time}, State) ->
    NewState = State#state{streaming_reports =
        [{ActionName, Time}] ++ State#state.streaming_reports},
    io:format("Results = ~p~n", [NewState#state.streaming_reports]),
    {noreply, NewState};

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
create_gunners(GunnerList, 0) ->
    GunnerList;

create_gunners(GunnerList, Count) ->
    {ok, GunnerPID} = barrage_gunner:start(),
    ListName = "s_" ++ integer_to_list(Count),
    AtomName = list_to_atom(ListName),
    barrage_gunner:set_httpc_profile(GunnerPID, AtomName),
    NewList = [GunnerPID | GunnerList],
    create_gunners(NewList, Count - 1).

destroy_gunners(Gunners) ->
    DestoryFun = fun(Pid) ->
        barrage_gunner:stop(Pid) 
    end,
    %Have all the gunners sight in the target
    lists:foreach(DestoryFun, Gunners).
