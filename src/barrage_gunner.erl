%%%-------------------------------------------------------------------
%%% @author Chris Jimison
%%% @doc
%%%     This is a client instance.  It should be unnamed
%%%     and allow to run many worker versions of it.
%%% @end
%%% Created : 2013-04-02 11:47:46.533194
%%%-------------------------------------------------------------------
-module(barrage_gunner).

-behaviour(gen_server).

%% API
-export([start/0]).
-export([start_link/0]).
-export([follow_order/2]).


%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts a unnamed server.  The idea here is you will start a bunch
%% of these and let them run through the load testing
%%
%% @spec start() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @doc
%% Tell this "client" to start running the tests.  Depending on the
%% behavior of the client behavior it will execute the Behavior tree
%% fully to execution of N cycles then kick out.
%%
%% @spec begin_behavior(Behavior) -> ok | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
follow_order(GunnerPid, Order) ->
    gen_server:cast(GunnerPid, {follow_order, Order}).


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
    A = crypto:rand_uniform(1, 9999999999999),
    B = crypto:rand_uniform(1, 9999999999999),
    C = crypto:rand_uniform(1, 9999999999999),
    random:seed(A, B, C),
    {ok, #state{}}.

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
handle_cast({follow_order, Order}, State) ->
    % This is where we will start to multiplex out the system
    process_set([Order]),
    {noreply, State};

handle_cast(_Msg, State) ->
    io:format("Why did I hit?~n~p~n", [_Msg]),
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Unpack the data and send the action off for processing
%%
%% @spec 
%% @end
%%--------------------------------------------------------------------
process_set(undefined) ->
    ok;
process_set([]) ->
    ok;
process_set(Set) ->
    [Order| Orders] = Set,
    Action      = proplists:get_value(action, Order),
    Children    = proplists:get_value(children, Order),
    Args        = proplists:get_value(args, Order),
    do_action(Action, Args, Children),
    process_set(Orders),
    ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Do the action described by the system
%%
%% @spec 
%% @end
%%--------------------------------------------------------------------
now_micro() -> 
    {MegaSecs,Secs,MicroSecs} = now(),
    (MegaSecs*1000000 + Secs)*1000000 + MicroSecs. 

%%%%------------------------------------------------------------------
%%%% do_random_count
%%%%------------------------------------------------------------------
do_random_count(_Children, 0) ->
    ok;
do_random_count(Children, Count) ->
    Idx = random:uniform(length(Children)),
    Child = lists:nth(Idx, Children),
    process_set([Child]),
    do_random_count(Children, Count -1).

%%%%------------------------------------------------------------------
%%%% do_random_timed
%%%%------------------------------------------------------------------
do_random_timed(Children, TimeEnd) ->
    Idx = random:uniform(length(Children)),
    Child = lists:nth(Idx, Children),
    process_set([Child]),
    Rez = TimeEnd - now_micro(),
    case Rez > 0 of
        true ->
            do_random_timed(Children, TimeEnd);
        _ ->
            ok
    end.

%%%%------------------------------------------------------------------
%%%% do_wait and do_random_wait
%%%%------------------------------------------------------------------
do_wait(Children, Time) ->
    timer:sleep(Time),
    process_set(Children).

do_random_wait(Children, undefined) ->
    do_random_wait(Children, 100);

do_random_wait(Children, Time) ->
    Sleep_time = random:uniform(Time),
    do_wait(Children, Sleep_time).

%%%%------------------------------------------------------------------
%%%% Action: <<"random">>
%%%%------------------------------------------------------------------
do_action(<<"random">>, undefined, Children) ->
    do_random_count(Children, 1);

do_action(<<"random">>, Args, Children) ->
    Count = proplists:get_value(count, Args),
    case Count of
        undefined ->
            Time = proplists:get_value(min_time, Args),
            TimeEnd = now_micro() + (Time * 1000),
            do_random_timed(Children, TimeEnd);
        _ ->
            do_random_count(Children, Count)
    end;

%%%%------------------------------------------------------------------
%%%% Action: <<"wait">>
%%%%------------------------------------------------------------------

do_action(<<"wait">>, undefined, Children) ->
    do_wait(Children, 100);

do_action(<<"wait">>, Args, Children) ->
    Count = proplists:get_value(time, Args),
    do_wait(Children, Count);

%%%%------------------------------------------------------------------
%%%% Action: <<"random_wait">>
%%%%------------------------------------------------------------------

do_action(<<"random_wait">>, undefined, Children) ->
    do_random_wait(Children, 100);

do_action(<<"random_wait">>, Args, Children) ->
    Count = proplists:get_value(time, Args),
    do_random_wait(Children, Count);

%%%%------------------------------------------------------------------
%%%% Action: User Defined
%%%%------------------------------------------------------------------
do_action(ActionName, Args, undefined) ->
    do_action(ActionName, Args, []);

do_action(ActionName, _Args, Children) ->
    % Pull out the from the ets table
    TableData = ets:lookup(actions, ActionName),
    case TableData of
        [] ->
            not_found;
        _ ->
            [{_, Action}]   = TableData,
            execute_action(Action),
            process_set(Children)
    end,
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Prepare the "get" based arguments for processing
%%
%% @spec 
%% @end
%%--------------------------------------------------------------------
prepare_get_args(Head, []) ->
    Head;
prepare_get_args(Head, Args) ->
    create_get_string(Head, Args, "?"). 

create_get_string(Head, Args, Token) ->
    [{ArgName, ArgValue} | TArgs] = Args,
    case TArgs of
        [] ->
            % That is all folks, lets boggie out
            string:join([Head, Token, ArgName, "=", ArgValue], "");
        _ ->
            H1 = string:join([Head, Token, ArgName, "=", ArgValue], ""),
            create_get_string(H1, TArgs, "&")
    end.
    

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% Do the actual http request sendoff
%%  {[
%%    {name, "<Name of Action>"},
%%    {url, "<URL to hit>"},
%%    {type, http_type},    <-- atom typeof the html request
%%    {args_type, http},    <-- how should the args be send (http or json)
%%    {args, [
%%          ...             <-- List of key value pairs
%%    ]},
%%    {results_type, json}  <-- Output format (http or json)
%%    {results,[ 
%%          ...             <-- Key value pairs.  For http there can be only one
%%    ]},
%%  ]}.
%% 
%% @spec 
%% @end
%%--------------------------------------------------------------------
execute_action(Action) ->
    BaseURL = proplists:get_value(url, Action),
    Method  = proplists:get_value(type,Action),
    Header  = [],
    HTTPOps = [],
    Ops     = [],
    Args    = proplists:get_value(args, Action),
    
    case proplists:get_value(type, Action) of
        get ->
            URL             = prepare_get_args(BaseURL, Args),
            {_Time, _Res}   = timer:tc(httpc, request,
                                  [Method, {URL, Header}, HTTPOps, Ops]),

            % What am I doing with the results of the test?
            ok;
        post ->
            not_implemented;
        _ ->
            not_supported
    end.

