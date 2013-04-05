%%%-------------------------------------------------------------------
%%% @author Chris Jimison
%%% @copyright (C) 2013, Not Rigged Games LLC
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
    {Data}      = Order,
    Action      = proplists:get_value(action, Data),
    Children    = proplists:get_value(children, Data),
    Args        = proplists:get_value(args, Data),
    do_action(Action, Args, Children),
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

do_action(<<"random">>, undefined, _Children) ->
    error;
do_action(<<"random">>, _Args, _Children) ->
    ok;
do_action(ActionName, _Args, _Children) ->
    % Pull out the from the ets table
    TableData = ets:lookup(actions, ActionName),
    case TableData of
        [] ->
            not_found;
        _ ->
            [{_, Action}]   = TableData,
            execute_action(Action)
    end,
    ok.

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
            URL         = prepare_get_args(BaseURL, Args),
            {_Time, _Res} = timer:tc(httpc, request,
                                  [Method, {URL, Header}, HTTPOps, Ops]),
            ok;
        post ->
            not_implemented;
        _ ->
            not_supported
    end.

