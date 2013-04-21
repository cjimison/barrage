%%%-------------------------------------------------------------------
%%% @author Chris Jimison
%%% @copyright (c) 2013 Christopher Jimison
%%%
%%% Permission is hereby granted,free of charge, to any person obtaining 
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
%%% MERCHANTABILITY,FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
%%% IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY 
%%% CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, 
%%% TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE 
%%% SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%% @doc
%%%     This is a client instance.  It should be unnamed
%%%     and allow to run many worker versions of it.
%%% @end
%%% Created : 2013-04-19 15:32:55.479780
%%%-------------------------------------------------------------------
-module(barrage_action_http).


%% API
-export([execute/2]).

-record(state, {url, results, keystore, inets_pid, profile=default}).

%%%===================================================================
%%% Public API
%%%===================================================================



%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Prepare the "get" based arguments for processing
%%
%% @spec 
%% @end
%%--------------------------------------------------------------------
prepare_get_args(Head, undefined, _Store) ->
    Head;
prepare_get_args(Head, [], _Store) ->
    Head;
prepare_get_args(Head, Args, Store) ->
    {TheArgs} = Args,
    create_get_string(Head, TheArgs, <<"?">>, Store). 

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Prepare the "post" based arguments for processing
%%
%% @spec 
%% @end
%%--------------------------------------------------------------------
prepare_post_args(undefined, _Store) ->
    <<"">>;
prepare_post_args(Args, Store) ->
    {TheArgs} = Args,
    create_get_string(<<"">>, TheArgs, <<"">>, Store). 

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Builds the form encoded arguments string
%%
%% @spec 
%% @end
%%--------------------------------------------------------------------
build_string(TArgs, Head, Token, ArgName, ArgValue, Store) 
    when is_integer(ArgValue) ->
    IntArgVal = list_to_binary(integer_to_list(ArgValue)),
    build_string(TArgs, Head, Token, ArgName, IntArgVal, Store);

build_string(TArgs, Head, Token, ArgName, ArgValue, Store)
    when is_float(ArgValue) ->
    FloatArgVal = list_to_binary(float_to_list(ArgValue)),
    build_string(TArgs, Head, Token, ArgName, FloatArgVal, Store);

build_string(TArgs, Head, Token, ArgName, ArgValue, Store)
    when is_list(ArgValue) ->
    ListArgVal = list_to_binary(ArgValue),
    build_string(TArgs, Head, Token, ArgName, ListArgVal, Store);

build_string(TArgs, Head, Token, ArgName, ArgValue, Store)
    when is_atom(ArgValue) ->
    AtomArgVal = list_to_binary(atom_to_list(ArgValue)),
    build_string(TArgs, Head, Token, ArgName, AtomArgVal, Store);

build_string(TArgs, Head, Token, ArgName, ArgValue, Store)
    when is_binary(ArgValue) ->
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
            create_get_string(H1, TArgs, <<"&">>, Store)
    end.

create_get_string(Head, Args, Token, Store) ->
    [{ArgName, ArgValue} | TArgs] = Args,
    <<Tag:1/binary, _TagData/binary>> = ArgValue,
    case Tag of
        <<"$">> ->
            IsKey = dict:is_key(ArgValue, Store),
            case IsKey of
                true ->
                    NewArgValue = dict:fetch(ArgValue, Store),
                    build_string(   TArgs, Head, Token, 
                                    ArgName, NewArgValue, Store);
                _ ->
                    build_string(   TArgs, Head, Token, 
                                    ArgName, ArgValue, Store)
            end;
        _ ->
            build_string(TArgs, Head, Token, ArgName, ArgValue, Store)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%%  Write the results to the process calls state ref
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Builds the form encoded arguments string
%%
%% @spec 
%% @end
%%--------------------------------------------------------------------
process_keydata(_, _, [], Store) ->
    Store;
process_keydata(Results, Data, Keys, Store) ->
    [Key | OtherKeys] = Keys,
    KeyValue = proplists:get_value(Key, Data),
    NewKeyName = proplists:get_value(Key, Results),
    NewStore = dict:store(NewKeyName, KeyValue, Store),
    process_keydata(Results, Data, OtherKeys, NewStore).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Process the output from the game server
%%
%% @spec 
%% @end
%%--------------------------------------------------------------------
process_action_results(_, undefined, _, State) ->
    State;

process_action_results(_, _, undefined, State) ->
    State;

process_action_results(Result, <<"json">>, Results, State) ->
    {_, {_,Info, JsonData}} = Result,
    httpc:store_cookies(Info, 
                        binary_to_list(State#state.url), 
                        State#state.profile),

    %% TODO  This is still a large point of failure
    %% If the server returns invalid json then this whole client
    %% is screwed up.  However for this demo pass I am cool
    %% with things being a little less robust but I need to fix
    %% it before I move into first production phase
    {Data}          = jiffy:decode(JsonData),
    {TheResults}    = Results,
    Keys            = proplists:get_keys(TheResults),
    Store           = State#state.keystore,
    NKS             = process_keydata(TheResults, Data, Keys, Store),
    State#state{keystore=NKS};

process_action_results(_Result, _Type, _Results, State) ->
    State.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% Do the actual http request sendoff
%%  {[
%%    {name, "<Name of Action>"},
%%    {url, "<URL to hit>"},
%%    {type, http_type},    <-- atom typeof the html request
%%    {args_type, http},    <-- how should the args be send(http or json)
%%    {args, [
%%          ...             <-- List of key value pairs
%%    ]},
%%    {results_type, json}  <-- Output format (http or json)
%%    {results,[ 
%%          ...             <-- Key value pairs.For http there can be 
%%                              only one
%%    ]},
%%  ]}.
%% 
%% @spec 
%% @end
%%--------------------------------------------------------------------
execute(Action, State) ->
    ActionName  = proplists:get_value(<<"name">>, Action),
    HeadURL     = State#state.url,
    TailURL     = proplists:get_value(<<"url">>, Action), 
    BaseURL     = <<HeadURL/binary, TailURL/binary>>,
    Header      = [],
    HTTPOps     = [],
    Ops         = [],

    %%% Optional Defined
    Args        = proplists:get_value(<<"args">>, Action),
    Results     = proplists:get_value(<<"results">>, Action),
    ResultsType = proplists:get_value(<<"results_type">>, Action),
    
    case proplists:get_value(<<"type">>, Action) of

        <<"get">> ->
            URL             = prepare_get_args( BaseURL, 
                                                Args,
                                                State#state.keystore),
            LURL            = binary_to_list(URL),
            {Time, Result}  = timer:tc(httpc, request, [
                                    get, 
                                    {LURL, Header}, 
                                    HTTPOps, Ops, State#state.profile]),
            NewState = process_action_results(  Result, 
                                                ResultsType,
                                                Results,
                                                State),
            store_action_results(ActionName, Time, NewState);

        <<"post">> ->
            Type            = "application/x-www-form-urlencoded",
            Body            = prepare_post_args(Args, 
                                                State#state.keystore),
            {Time, Result}  = timer:tc(httpc, request, [
                                    post, 
                                    {
                                        binary_to_list(BaseURL), 
                                        Header, 
                                        Type, 
                                        binary_to_list(Body)
                                    }, 
                                    HTTPOps, Ops, State#state.profile]),
            NewState = process_action_results(  Result,
                                                ResultsType,
                                                Results,
                                                State),
            store_action_results(ActionName, Time, NewState);

        <<"post_json">> ->
            Type            = "application/json",
            Body            = binary_to_list(jiffy:encode(Args)),
            {Time, Result}  = timer:tc(httpc, request, [
                                    post, 
                                    {
                                        binary_to_list(BaseURL), 
                                        Header, 
                                        Type, 
                                        Body
                                    }, 
                                    HTTPOps, Ops, State#state.profile]),
            NewState = process_action_results(  Result,
                                                ResultsType,
                                                Results,
                                                State),
            store_action_results(ActionName, Time, NewState);

        <<"post_multipart">> ->
            State;

        _ ->
            % Undefined type of post
            State
    end.

