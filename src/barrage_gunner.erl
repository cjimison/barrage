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
%%%     This is a client instance.  It should be unnamed
%%%     and allow to run many worker versions of it.
%%% @end
%%% Created : 2013-04-02 11:47:46.533194
%%%-------------------------------------------------------------------
-module(barrage_gunner).

-behaviour(gen_server).

%% API
-export([start/0]).
-export([stop/1]).
-export([start_link/0]).
-export([follow_order/2]).
-export([set_server_info/3]).
-export([set_httpc_profile/2]).


%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-include("barrage_gunner.hrl").

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

stop(GunnerPid) ->
    gen_server:call(GunnerPid, stop, infinity).

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

set_server_info(GunnerPid, Server, Port) ->
    gen_server:call(GunnerPid, {set_server_info, Server, Port}, infinity).

set_httpc_profile(GunnerPid, Profile) ->
    gen_server:call(GunnerPid, {set_httpc_profile, Profile}, infinity).

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
handle_call({set_server_info, Server, Port}, _From, State) ->
    NewState = State#state{server=Server, port=Port},
    Reply = ok,
    {reply, Reply, NewState};

handle_call({set_httpc_profile, Profile}, _From, State) ->
    {ok, PID} = inets:start(httpc, [{profile, Profile}]),
    NewState = State#state{profile=Profile, inets_pid=PID},
    {reply, ok, NewState};

handle_call(stop, _From, State) ->
    inets:stop(httpc, State#state.inets_pid),
    {stop, normal, ok, State};

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
    try 
        httpc:set_options([{cookies, enabled}], State#state.profile),
        httpc:reset_cookies(State#state.profile),    
        httpc:set_options([{cookies, enabled}], State#state.profile),
        State2 = State#state{results=dict:new(), keystore=dict:new()},
        State3 = process_set([Order], State2),
        barrage_commander:order_complete(self(), State3#state.results),
        {noreply, State}
    catch Exception:Reason -> 
        io:format("Gunner Exception.  Bail out of this Tree~nException: ~p ~nReason: ~p ~nStacktrace: ~p ~n", [Exception,Reason,erlang:get_stacktrace()]), 
        barrage_commander:order_complete(self(), dict:new()),
        {noreply, State}
    end;


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
    [{Order}| Orders] = Set,
    
    case proplists:get_value(<<"action">>, Order) of
        undefined ->
            Behavior    = proplists:get_value(<<"behavior">>, Order),
            [{_,BO}]    = ets:lookup(plans, Behavior),
            NextState   = process_set([BO], State),
            process_set(Orders, NextState);
        Action ->
            Children    = proplists:get_value(<<"children">>, Order),
            Args        = proplists:get_value(<<"args">>, Order),
            NewState    = do_action(Action, Args, Children, State),
            process_set(Orders, NewState)
    end.

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
            do_ordered_count(Children, NewIdx, Count, NewState)
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
    Idx         = random:uniform(length(Children)),
    Child       = lists:nth(Idx, Children),
    NewState    = process_set([Child], State),
    do_random_count(Children, Count -1, NewState).

%%%%------------------------------------------------------------------
%%%% do_random_timed
%%%%------------------------------------------------------------------
do_random_timed(Children, TimeEnd, State) ->
    Idx         = random:uniform(length(Children)),
    Child       = lists:nth(Idx, Children),
    NewState    = process_set([Child], State),
    Rez         = TimeEnd - now_micro(),
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

do_action(<<"random">>, {Args}, Children, State) ->
    case proplists:get_value(<<"count">>, Args) of
        undefined ->
            Time    = proplists:get_value(<<"min_time">>, Args),
            TimeEnd = now_micro() + (Time * 1000),
            do_random_timed(Children, TimeEnd, State);
        Count when is_integer(Count) ->
            do_random_count(Children, Count, State);
        Count when is_binary(Count) ->
            <<Tag:1/binary, Name/binary>> = Count,
            case Tag of
                <<"$">> ->
                    Value = dict:fetch(Name, State#state.keystore),
                    % Lets pull this guy out of the KVS
                    do_random_count(Children, Value, State)
            end
    end;

%%%%------------------------------------------------------------------
%%%% Action: <<"random">>
%%%%------------------------------------------------------------------
do_action(<<"ordered">>, undefined, Children, State) ->
    do_ordered_count(Children, 1, 1, State);

do_action(<<"ordered">>, {Args}, Children, State) ->
    case proplists:get_value(<<"count">>, Args) of
        undefined ->
            Time    = proplists:get_value(<<"min_time">>, Args),
            TimeEnd = now_micro() + (Time * 1000),
            do_ordered_timed(Children, 1, TimeEnd, State);
        Count when is_integer(Count) ->
            do_ordered_count(Children, 1, Count, State);
        Count when is_binary(Count) ->
            io:format("Pulling out the Count baby~n"),
            <<Tag:1/binary, Name/binary>> = Count,
            case Tag of
                <<"$">> ->
                    Value = dict:fetch(Name, State#state.keystore),
                    io:format("Doing this shit ~p times ~n", [Value]),
                    % Lets pull this guy out of the KVS
                    do_ordered_count(Children, 1, Value, State)
            end
    end;

%%%%------------------------------------------------------------------
%%%% Action: <<"wait">>
%%%%------------------------------------------------------------------
do_action(<<"wait">>, undefined, Children, State) ->
    do_wait(Children, 100, State);

do_action(<<"wait">>, {Args}, Children, State) ->
    Count = proplists:get_value(<<"time">>, Args),
    do_wait(Children, Count, State);

%%%%------------------------------------------------------------------
%%%% Action: <<"random_wait">>
%%%%------------------------------------------------------------------
do_action(<<"random_wait">>, undefined, Children, State) ->
    do_random_wait(Children, 100, State);

do_action(<<"random_wait">>, {Args}, Children, State) ->
    Count = proplists:get_value(<<"time">>, Args),
    do_random_wait(Children, Count, State);

%%%%------------------------------------------------------------------
%%%% Action: <<"set_variable">>
%%%%------------------------------------------------------------------
do_action(<<"set_variable">>, undefined, _Children, State) ->
    State;

do_action(<<"set_variable">>, {Args}, _Children, State) ->
    Key     = proplists:get_value(<<"key">>, Args),
    Value   = proplists:get_value(<<"value">>, Args),
    NewDict = dict:store(Key, Value, State#state.keystore),
    State#state{keystore=NewDict};

%%%%------------------------------------------------------------------
%%%% Action: <<"inc_variable">>
%%%%------------------------------------------------------------------
do_action(<<"inc_variable">>, undefined, _Children, State) ->
    State;

do_action(<<"inc_variable">>, {Args}, _Children, State) ->
    Key     = proplists:get_value(<<"key">>, Args),
    Value   = dict:fetch(Key, State#state.keystore),
    NewDict = dict:store(Key, Value + 1, State#state.keystore),
    State#state{keystore=NewDict};

%%%%------------------------------------------------------------------
%%%% Action: <<"dec_variable">>
%%%%------------------------------------------------------------------
do_action(<<"dec_variable">>, undefined, _Children, State) ->
    State;

do_action(<<"dec_variable">>, {Args}, _Children, State) ->
    Key     = proplists:get_value(<<"key">>, Args),
    Value   = dict:fetch(Key, State#state.keystore),
    NewDict = dict:store(Key, Value - 1, State#state.keystore),
    State#state{keystore=NewDict};

%%%%------------------------------------------------------------------
%%%% Action: <<"print_variable">>
%%%%------------------------------------------------------------------
do_action(<<"print_variable">>, undefined, _Children, State) ->
    State;

do_action(<<"print_variable">>, {Args}, _Children, State) ->
    Key     = proplists:get_value(<<"key">>, Args),
    Value   = dict:fetch(Key, State#state.keystore),
    io:format("Variable ~p = ~p~n", [Key, Value]),
    State;

%%%%------------------------------------------------------------------
%%%% Action: <<"load_kv_store">>
%%%%------------------------------------------------------------------
do_action(<<"load_kv_store">>, undefined, _Children, State) ->
    State;

do_action(<<"load_kv_store">>, {Args}, _Children, State) ->
    Profile = proplists:get_value(<<"kv_store">>, Args),
    File    = proplists:get_value(<<"file">>, Args),
    barrage_action_kvs:load(Profile, File, State);


%%%%------------------------------------------------------------------
%%%% Action: <<"load_array_store">>
%%%%------------------------------------------------------------------
do_action(<<"load_array_store">>, undefined, _Children, State) ->
    State;

do_action(<<"load_array_store">>, {Args}, _Children, State) ->
    Profile = proplists:get_value(<<"array_store">>, Args),
    File    = proplists:get_value(<<"file">>, Args),
    barrage_action_kvs:load_array(Profile, File, State);

%%%%------------------------------------------------------------------
%%%% Action: <<"read_random_kv">>
%%%%------------------------------------------------------------------
do_action(<<"read_random_kv">>, undefined, _Children, State) ->
    State;

do_action(<<"read_random_kv">>, {Args}, _Children, State) ->
    Profile = proplists:get_value(<<"kv_store">>, Args),
    Key     = proplists:get_value(<<"key">>, Args),
    Value   = proplists:get_value(<<"value">>, Args),
    barrage_action_kvs:read_random(Profile, Key, Value, State);  


%%%%------------------------------------------------------------------
%%%% Action: <<"read_random_number">>
%%%%------------------------------------------------------------------
do_action(<<"read_random_value">>, undefined, _Children, State) ->
    State;

do_action(<<"read_random_value">>, {Args}, _Children, State) ->
    Min     = proplists:get_value(<<"min">>, Args),
    Max     = proplists:get_value(<<"max">>, Args),
    Preffix = proplists:get_value(<<"preffix">>, Args),
    NumberKey  = proplists:get_value(<<"number">>, Args),
    NameKey    = proplists:get_value(<<"name">>, Args),
    
    Number  = Min + random:uniform(Max - Min),
    % $TODO this doesn't work for 15B or less. 
    %BinNumber = integer_to_binary(Number),
    BinNumber = list_to_binary(integer_to_list(Number)),
    Name    = <<Preffix/binary, BinNumber/binary>>, 
    NewDic  = dict:store(NumberKey, Number, State#state.keystore),
    UpdatedDic  = dict:store(NameKey, Name, NewDic),    
    State#state{keystore = UpdatedDic};

%%%%------------------------------------------------------------------
%%%% Action: <<"read_named_kv">>
%%%%------------------------------------------------------------------
do_action(<<"read_named_kv">>, undefined, _Children, State) ->
    State;

do_action(<<"read_named_kv">>, {Args}, _Children, State) ->
    Profile = proplists:get_value(<<"kv_store">>, Args),
    Key     = proplists:get_value(<<"key">>, Args),
    Variable= proplists:get_value(<<"variable">>, Args),
    barrage_action_kvs:read_name(Profile, Key, Variable, State);

%%%%------------------------------------------------------------------
%%%% Action: <<"store_to_kv">>
%%%%------------------------------------------------------------------
do_action(<<"store_to_kv">>, undefined, _Children, State) ->
    State;

do_action(<<"store_to_kv">>, {Args}, _Children, State) ->
    Profile = proplists:get_value(<<"kv_store">>, Args),
    Variable= proplists:get_value(<<"variable">>, Args),
    Key     = proplists:get_value(<<"key">>, Args),
    barrage_action_kvs:read_name(Variable, Profile, Key, State);

%%%%------------------------------------------------------------------
%%%% Action: <<"read_array_idx">>
%%%%------------------------------------------------------------------
do_action(<<"read_array_idx">>, undefined, _Children, State) ->
    State;

do_action(<<"read_array_idx">>, {Args}, _Children, State) ->
    Key         = proplists:get_value(<<"key">>, Args),
    Idx         = proplists:get_value(<<"idx">>, Args),
    Variable    = proplists:get_value(<<"variable">>, Args),
    NewIdx      = get_stored_value(Idx, State),
    {ok, Array} = dict:find( Key, State#state.keystore),
    case is_binary(NewIdx) of
        true ->
            LIdx= binary_to_list(NewIdx),
            IIdx= list_to_integer(LIdx), 
            NewData     = lists:nth(IIdx, Array),
            NewDic      = dict:store(Variable, NewData, State#state.keystore),
            State#state{keystore = NewDic};
        false ->
            case is_list(NewIdx) of
                true ->
                    IIdx= list_to_integer(NewIdx), 
                    NewData     = lists:nth(IIdx, Array),
                    NewDic      = dict:store(Variable, NewData, State#state.keystore),
                    State#state{keystore = NewDic};
                false ->
                    NewData     = lists:nth(NewIdx, Array),
                    NewDic      = dict:store(Variable, NewData, State#state.keystore),
                    State#state{keystore = NewDic}
            end
    end;

%%%%------------------------------------------------------------------
%%%% Action: <<"read_doc_set_idx">>
%%%%------------------------------------------------------------------
do_action(<<"read_doc_set_idx">>, undefined, _Children, State) ->
    State;

do_action(<<"read_doc_set_idx">>, {Args}, _Children, State) ->
    Key         = proplists:get_value(<<"key">>, Args),
    Idx         = proplists:get_value(<<"idx">>, Args),
    Variable    = proplists:get_value(<<"variable">>, Args),
    NewIdx      = get_stored_value(Idx, State),
    {ok, {Array}} = dict:find( Key, State#state.keystore),
    case is_binary(NewIdx) of
        true ->
            LIdx= binary_to_list(NewIdx),
            IIdx= list_to_integer(LIdx),
            {_,NewData} = lists:nth(IIdx, Array),
            NewDic      = dict:store(Variable, NewData, State#state.keystore),
            State#state{keystore = NewDic};
        false ->
            case is_list(NewIdx) of
                true ->
                    IIdx= list_to_integer(NewIdx), 
                    {_,NewData} = lists:nth(IIdx, Array),
                    NewDic      = dict:store(Variable, NewData, State#state.keystore),
                    State#state{keystore = NewDic};
                false ->
                    NewData     = lists:nth(NewIdx, Array),
                    NewDic      = dict:store(Variable, NewData, State#state.keystore),
                    State#state{keystore = NewDic}
            end
    end;

%%%%------------------------------------------------------------------
%%%% Action: <<"read_sequential_array_idx">>
%%%%------------------------------------------------------------------
do_action(<<"read_sequential_array_idx">>, undefined, _Children, State) ->
    State;
do_action(<<"read_sequential_array_idx">>, {Args}, _Children, State) ->
    Key         = proplists:get_value(<<"key">>, Args),
    Variable    = proplists:get_value(<<"variable">>, Args),
    CurIdxKey   = <<Key/binary,"_CURRENT_IDX">>,
    CurIdx = case dict:find(CurIdxKey, State#state.keystore) of
                {ok, FoundValue} -> FoundValue;
                error -> 0
             end,
    {ok, Array} = dict:find(Key, State#state.keystore),
    case length(Array) of 
        0 ->
            State;
        Len ->
            Idx         = CurIdx rem Len,
            NewData     = lists:nth(Idx+1, Array),
            NewDic      = dict:store(Variable, NewData, State#state.keystore),
            UpdatedDic  = dict:store(CurIdxKey, Idx+1, NewDic),
            State#state{keystore = UpdatedDic}
    end;

%%%%------------------------------------------------------------------
%%%% Action: <<"read_random_array_idx">>
%%%%------------------------------------------------------------------
do_action(<<"read_random_array_idx">>, undefined, _Children, State) ->
    State;
do_action(<<"read_random_array_idx">>, {Args}, _Children, State) ->
    Key         = proplists:get_value(<<"key">>, Args),
    Variable    = proplists:get_value(<<"variable">>, Args),

    {ok, Array} = dict:find(Key, State#state.keystore),
    case length(Array) of 
        0 ->
            State;
        Len ->
            Idx         = random:uniform(Len),
            NewData     = lists:nth(Idx, Array),
            NewDic      = dict:store(Variable, NewData, State#state.keystore),
            State#state{keystore = NewDic}
    end;


%%%%------------------------------------------------------------------
%%%% Action: <<"read_random_doc_set_idx">>
%%%%------------------------------------------------------------------
do_action(<<"read_random_doc_set_idx">>, undefined, _Children, State) ->
    State;
do_action(<<"read_random_doc_set_idx">>, {Args}, _Children, State) ->
    Key           = proplists:get_value(<<"key">>, Args),
    Variable      = proplists:get_value(<<"variable">>, Args),

    {ok, {Array}} = dict:find(Key, State#state.keystore),
    case length(Array) of 
        0 ->
            State;
        Len ->
            Idx          = random:uniform(Len),
            {_, NewData} = lists:nth(Idx, Array),
            NewDic       = dict:store(Variable, NewData, State#state.keystore),
            State#state{keystore = NewDic}
    end;


%%%%------------------------------------------------------------------
%%%% Action: <<"conditional">>
%%%%------------------------------------------------------------------
do_action(<<"conditional">>, undefined, _Children, State) ->
    State;

do_action(<<"conditional">>, {Args}, Children, State) ->
    [{Key, Value}]  = Args,
    ActualKey       = get_stored_value(Key, State),
    ValueKey        = get_stored_value(Value, State),
    case length(Children) of
        1 ->
            case (ActualKey == ValueKey) of 
                true ->
                    process_set([lists:nth(1, Children)], State);
                _ ->
                    State
            end;
        2 ->
            case (ActualKey == ValueKey) of 
                true ->
                    process_set([lists:nth(1, Children)], State);
                false ->
                    process_set([lists:nth(2, Children)], State)
            end;
        _->
            State 
    end;

%%%%------------------------------------------------------------------
%%%% Action: <<"print_state">>  This is a debugging action
%%%%------------------------------------------------------------------
do_action(<<"print_state">>, undefined, undefined, State) ->
    io:format("State of the gunner: SOME USEFUL INFO...~n"),
    State;


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
            NewState = barrage_action_http:execute(Action, State),
            process_set(Children, NewState)
    end.

%%%%------------------------------------------------------------------
%%%% get_stored_value
%%%% This will pull a named value out of the key storage
%%%%------------------------------------------------------------------
get_stored_value(Name, State) ->
    <<Tag:1/binary, _TagData/binary>> = Name,
    case Tag of
        <<"$">> ->
            case dict:is_key(Name, State#state.keystore) of
                true ->
                    dict:fetch(Name, State#state.keystore);
                _->
                    Name
            end;
        _->
            Name
    end.
