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
    % Create a list of commanders
    {ok, Commander1} = barrage_commander:start(),
    {ok, Commander2} = barrage_commander:start(),
    {ok, Commander3} = barrage_commander:start(),
    {ok, Commander4} = barrage_commander:start(),
    {ok, Commander5} = barrage_commander:start(),
   
    % Enlist them under this general
    barrage_general:enlist(Commander1),
    barrage_general:enlist(Commander2),
    barrage_general:enlist(Commander3),
    barrage_general:enlist(Commander4),
    barrage_general:enlist(Commander5),

    % Now issue an order out to the commanders for execution
    barrage_general:issue_order(<<"Simple Test">>).

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
    Fun = fun(Pid) -> barrage_commander:execute(Pid, Order) end,
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




