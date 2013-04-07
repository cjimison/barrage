%%%-------------------------------------------------------------------
%%% @author Chris Jimison
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
         execute/3]).

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
        general=null,
        gunners=[]
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
create_gunner(GunnerList, 0) ->
    GunnerList;
create_gunner(GunnerList, Count) ->
    {ok, GunnerPID} = barrage_gunner:start(),
    NewList = [GunnerPID | GunnerList],
    create_gunner(NewList, Count - 1). 

execute(Pid, Orders, TargetIP) ->
    gen_server:cast(Pid, {execute, {Orders, TargetIP}}).

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
    [{_, General}] = ets:lookup(barrage, general),
    rpc:call(General, barrage_general, enlist, [self()]),
    [{_, GunnerCount}] = ets:lookup(barrage, gunners),
    State = #state{general=null, gunners = create_gunner([], GunnerCount)},
    {ok, State}.


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
handle_cast({execute, {Order, TargetURL}}, State) ->
    %NOTE: I am currently doing this in two phases
    %      so the timing syncs up a little better.
    %      I may move this to a single phase step.
    %      REFACTOR: Have the follow_order take a
    %      target URL as well.
    FunTarget = fun(Pid) -> 
            barrage_gunner:set_url(Pid, TargetURL)
    end,

    FunFire = fun(Pid) ->
            barrage_gunner:follow_order(Pid, Order) 
    end,

    %Have all the gunners sight in the target
    lists:foreach(FunTarget, State#state.gunners),

    %The open fire!!!
    lists:foreach(FunFire, State#state.gunners),
    {noreply, State};

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

