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
-export([set_url/2]).


%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {url, results}).

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

set_url(GunnerPid, URL) ->
    gen_server:call(GunnerPid, {set_url, URL}).

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
    % This feels so hacky to me.  There has got to be a better way
    % So the 2147483646 is 1 less then the max value for a int32_t.  
    % My hope is to keep this within a reg value but we shall see.
    A = crypto:rand_uniform(1, 2147483646),
    B = crypto:rand_uniform(1, 2147483646),
    C = crypto:rand_uniform(1, 2147483646),
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
handle_call({set_url, URL}, _From, State) ->
    NewState = State#state{url=URL},
    Reply = ok,
    {reply, Reply, NewState};

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
    httpc:set_options([{cookies, enabled}]),
    httpc:reset_cookies(),    
    httpc:set_options([{cookies, enabled}]),
    State2 = State#state{results=dict:new()},
    State3 = process_set([Order], State2),
    barrage_commander:order_complete(self(), State3#state.results),
    {noreply, State3};

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
process_set(undefined, State) ->
    State;
process_set([], State) ->
    State;
process_set(Set, State) ->
    [Order| Orders] = Set,
    Action      = proplists:get_value(action, Order),
    Children    = proplists:get_value(children, Order),
    Args        = proplists:get_value(args, Order),
    NewState    = do_action(Action, Args, Children, State),
    process_set(Orders, NewState).

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
%%%% do_ordered_count
%%%%------------------------------------------------------------------
do_ordered_count(_Children, _Idx, 0, State) ->
    State;
do_ordered_count(Children, Idx, Count, State) ->
    NewState    = process_set([lists:nth(Idx, Children)], State),
    NewIdx      = Idx + 1,
    case NewIdx > length(Children) of
        true ->
            do_ordered_count(Children, 1, Count - 1, NewState);
        _ ->
            do_ordered_count(Children, NewIdx, Count - 1, NewState)
    end.

%%%%------------------------------------------------------------------
%%%% do_ordered_timed
%%%%------------------------------------------------------------------
do_ordered_timed(Children, Idx, TimeEnd, State) ->
    NewState = process_set([lists:nth(Idx, Children)], State),
    case (TimeEnd - now_micro()) > 0 of
        true ->
            NewIdx = Idx + 1,
            case NewIdx > length(Children) of
                true ->
                    do_ordered_timed(Children, 1, TimeEnd, NewState);
                _ ->
                    do_ordered_timed(Children, NewIdx, TimeEnd, NewState)
            end;
        _ ->
            NewState
    end.

%%%%------------------------------------------------------------------
%%%% do_random_count
%%%%------------------------------------------------------------------
do_random_count(_Children, 0, State) ->
    State;
do_random_count(Children, Count, State) ->
    Idx = random:uniform(length(Children)),
    Child = lists:nth(Idx, Children),
    NewState = process_set([Child], State),
    do_random_count(Children, Count -1, NewState).

%%%%------------------------------------------------------------------
%%%% do_random_timed
%%%%------------------------------------------------------------------
do_random_timed(Children, TimeEnd, State) ->
    Idx = random:uniform(length(Children)),
    Child = lists:nth(Idx, Children),
    NewState = process_set([Child], State),
    Rez = TimeEnd - now_micro(),
    case Rez > 0 of
        true ->
            do_random_timed(Children, TimeEnd, NewState);
        _ ->
            NewState
    end.

%%%%------------------------------------------------------------------
%%%% do_wait and do_random_wait
%%%%------------------------------------------------------------------
do_wait(Children, Time, State) ->
    timer:sleep(Time),
    process_set(Children, State).

do_random_wait(Children, undefined, State) ->
    do_random_wait(Children, 100, State);

do_random_wait(Children, Time, State) ->
    Sleep_time = random:uniform(Time),
    do_wait(Children, Sleep_time, State).

%%%%------------------------------------------------------------------
%%%% Action: <<"random">>
%%%%------------------------------------------------------------------
do_action(<<"random">>, undefined, Children, State) ->
    do_random_count(Children, 1, State);

do_action(<<"random">>, Args, Children, State) ->
    Count = proplists:get_value(count, Args),
    case Count of
        undefined ->
            Time = proplists:get_value(min_time, Args),
            TimeEnd = now_micro() + (Time * 1000),
            do_random_timed(Children, TimeEnd, State);
        _ ->
            do_random_count(Children, Count, State)
    end;

%%%%------------------------------------------------------------------
%%%% Action: <<"random">>
%%%%------------------------------------------------------------------
do_action(<<"ordered">>, undefined, Children, State) ->
    do_ordered_count(Children, 1, 1, State);

do_action(<<"ordered">>, Args, Children, State) ->
    Count = proplists:get_value(count, Args),
    case Count of
        undefined ->
            Time = proplists:get_value(min_time, Args),
            TimeEnd = now_micro() + (Time * 1000),
            do_ordered_timed(Children, 1, TimeEnd, State);
        _ ->
            do_ordered_count(Children, 1, Count, State)
    end;

%%%%------------------------------------------------------------------
%%%% Action: <<"wait">>
%%%%------------------------------------------------------------------

do_action(<<"wait">>, undefined, Children, State) ->
    do_wait(Children, 100, State);

do_action(<<"wait">>, Args, Children, State) ->
    Count = proplists:get_value(time, Args),
    do_wait(Children, Count, State);

%%%%------------------------------------------------------------------
%%%% Action: <<"random_wait">>
%%%%------------------------------------------------------------------

do_action(<<"random_wait">>, undefined, Children, State) ->
    do_random_wait(Children, 100, State);

do_action(<<"random_wait">>, Args, Children, State) ->
    Count = proplists:get_value(time, Args),
    do_random_wait(Children, Count, State);

%%%%------------------------------------------------------------------
%%%% Action: User Defined
%%%%------------------------------------------------------------------
do_action(ActionName, Args, undefined, State) ->
    do_action(ActionName, Args, [], State);

do_action(ActionName, _Args, Children, State) ->
    % Pull out the from the ets table
    TableData = ets:lookup(actions, ActionName),
    case TableData of
        [] ->
            State;
        _ ->
            [{_, Action}]   = TableData,
            NewState = execute_action(Action, State),
            process_set(Children, NewState)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Prepare the "get" based arguments for processing
%%
%% @spec 
%% @end
%%--------------------------------------------------------------------
prepare_get_args(Head, undefined) ->
    Head;
prepare_get_args(Head, []) ->
    Head;
prepare_get_args(Head, Args) ->
    create_get_string(Head, Args, <<"?">>). 

prepare_post_args(undefined) ->
    <<"">>;
prepare_post_args(Args) ->
    create_get_string(<<"">>, Args, <<"">>). 

create_get_string(Head, Args, Token) ->
    [{ArgName, ArgValue} | TArgs] = Args,
    case TArgs of
        [] ->
            % That is all folks, lets boggie out
            <<  Head/binary, 
                Token/binary, 
                ArgName/binary, 
                <<"=">>/binary, 
                ArgValue/binary>>;
        _ ->
            H1 = << Head/binary, 
                    Token/binary, 
                    ArgName/binary, 
                    <<"=">>/binary, 
                    ArgValue/binary>>,
            create_get_string(H1, TArgs, <<"&">>)
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
store_action_results(ActionName, Time, State) ->
    case dict:is_key(ActionName, State#state.results) of
        true ->
            State#state{results=dict:append(ActionName, 
                                            Time, 
                                            State#state.results)};
        _ -> 
            State#state{results=dict:store( ActionName, 
                                            [Time], 
                                            State#state.results)}
    end.

execute_action(Action, State) ->
    ActionName  = proplists:get_value(name, Action),
    HeadURL     = State#state.url,
    TailURL     = proplists:get_value(url, Action), 
    BaseURL     = <<HeadURL/binary, TailURL/binary>>,
    Method      = proplists:get_value(type,Action),
    Header      = [],
    HTTPOps     = [],
    Ops         = [],
    Args        = proplists:get_value(args, Action),
    
    case proplists:get_value(type, Action) of

        get ->
            URL             = prepare_get_args(BaseURL, Args),
            {Time, Result}  = timer:tc(httpc, request, [
                                    Method, 
                                    {binary_to_list(URL), Header}, 
                                    HTTPOps, Ops]),
            {_, {_,Info,_}} = Result,
            httpc:store_cookies(Info, binary_to_list(BaseURL)),
            store_action_results(ActionName, Time, State);

        post ->
            Type            = "application/x-www-form-urlencoded",
            Body            = prepare_post_args(Args),
            {Time, Result}  = timer:tc(httpc, request, [
                                    Method, 
                                    {
                                        binary_to_list(BaseURL), 
                                        Header, 
                                        Type, 
                                        binary_to_list(Body)
                                    }, 
                                    HTTPOps, Ops]),
            {_, {_,Info,_}} = Result,
            httpc:store_cookies(Info, binary_to_list(BaseURL)),
            store_action_results(ActionName, Time, State);

        post_json ->
            Type            = "application/json",
            Body            = binary_to_list(jiffy:encode({Args})),
            {Time, Result}  = timer:tc(httpc, request, [
                                    Method, 
                                    {
                                        binary_to_list(BaseURL), 
                                        Header, 
                                        Type, 
                                        Body
                                    }, 
                                    HTTPOps, Ops]),
            {_, {_,Info,_}} = Result,
            httpc:store_cookies(Info, binary_to_list(BaseURL)),
            store_action_results(ActionName, Time, State);

        post_multipart ->
            State;

        _ ->
            % Undefined type of post
            State
    end.

