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

-include("barrage_gunner.hrl").

%%%===================================================================
%%% Public API
%%%===================================================================

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Prepare the "post" based arguments for processing
%%
%% @spec 
%% @end
%%--------------------------------------------------------------------
encode_post_args(undefined, _Store) ->
    "";
encode_post_args({Args}, Store) ->
    encode_uri_args_value("", Args, "", Store);

encode_post_args(_Args, _Store) ->
    "".

get_protocol_type(undefined, State) ->
    binary_to_list(State#state.protocol);

get_protocol_type(Protocol, _State) ->
    binary_to_list(Protocol).

get_server_domain(undefined, State) ->
    binary_to_list(State#state.server);

get_server_domain(Server, _State) ->
    binary_to_list(Server).

get_port_value(undefined, State) ->
    State#state.port;

get_port_value(Port, _State) ->
    Port.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Builds the form encoded arguments string
%%
%% @spec 
%% @end
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Prepare the "get" based arguments for processing
%%
%% @spec 
%% @end
%%--------------------------------------------------------------------
encode_uri_args(Head, undefined, _Store) ->
    Head;
encode_uri_args(Head, {Args}, Store) ->
    encode_uri_args_value(Head, Args, "?", Store); 
encode_uri_args(Head, _Args, _Store) ->
    Head.

encode_uri_args_value(URL, Args, Token, ClientStore) ->
    [{ArgName, ArgValue} | TArgs] = Args,
    <<Tag:1/binary, _TagData/binary>> = ArgValue,
    case Tag of
        <<"$">> ->
            IsKey = dict:is_key(ArgValue, ClientStore),
            case IsKey of
                true ->
                    NewArgValue = dict:fetch(ArgValue, ClientStore),
                    encode_get_args(TArgs, URL, Token, binary_to_list(ArgName), NewArgValue, ClientStore);
                _ ->
                    encode_get_args(TArgs, URL, Token, binary_to_list(ArgName), ArgValue, ClientStore)
            end;
        _ ->
            encode_get_args(TArgs, URL, Token, binary_to_list(ArgName), ArgValue, ClientStore)
    end.

encode_get_args(Args, URL, Token, ArgName, ArgValue, Store) when is_binary(ArgValue) ->
    encode_get_args(Args, URL, Token, ArgName, binary_to_list(ArgValue), Store); 
    
encode_get_args(Args, URL, Token, ArgName, ArgValue, Store) ->
    NURL = lists:concat([URL, Token, ArgName, "=" , ArgValue]),
    case Args of
        [] ->
            NURL;
        _ ->
            encode_uri_args_value(NURL, Args, "&", Store)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Replace all found Store keys in Text by their corresponding values
%% 
%% @spec 
%% @end
%%--------------------------------------------------------------------
replace_tokens_in_text(Text, Store) ->
    dict:fold( fun replace_one_key/3, Text, Store ).


%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Replace a Key by its Value if the key is found in Text
%% 
%% @spec 
%% @end
%%--------------------------------------------------------------------
replace_one_key(Key, Value, Text) ->
    << FirstChar:1/binary , _Rest/binary >> = Key,
    case FirstChar of
        <<"$">> when is_binary(Value) ->
            EscapedKey = "\\" ++ binary_to_list(Key),
            re:replace(Text, "\"" ++ EscapedKey ++ "\"", "\"" ++ binary_to_list(Value) ++ "\"", [ {return, list}, global ] );
        _   ->
            Text
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
    barrage_commander:log_results(ActionName, Time),
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
process_action_results(_, undefined, _, _, State) ->
    State;

process_action_results(_, _, undefined, _, State) ->
    State;

process_action_results(Result, <<"query_str">>, Results, URL, State) ->
    {_, {_,Info, QData}} = Result,
    httpc:store_cookies(Info, 
                        URL, 
                        State#state.profile),

    Data = cowboy_http:x_www_form_urlencoded(list_to_binary(QData)),
    {TheResults}    = Results,
    Keys            = proplists:get_keys(TheResults),
    Store           = State#state.keystore,
    NKS             = process_keydata(TheResults, Data, Keys, Store),
    State#state{keystore=NKS};

process_action_results(Result, <<"json">>, Results, URL, State) ->
    {_, {_,Info, JsonData}} = Result,
    httpc:store_cookies(Info, 
                        URL, 
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

process_action_results(Result, <<"json_obj">>, Results, URL, State) ->
    {_, {_,Info, JsonData}} = Result,
    httpc:store_cookies(Info, 
                        URL, 
                        State#state.profile),

    %% TODO  This is still a large point of failure
    %% If the server returns invalid json then this whole client
    %% is screwed up.  However for this demo pass I am cool
    %% with things being a little less robust but I need to fix
    %% it before I move into first production phase
    {Data}          = jiffy:decode(JsonData),
    NewStore = dict:store(Results, Data, State#state.keystore),
    State#state{keystore=NewStore};

process_action_results(_Result, _Type, _Results, _URL, State) ->
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
execute(Action, State) when 
    is_record(State, state) andalso
    is_list(Action) ->

    ActionName  = proplists:get_value(<<"name">>, Action),
   
    Protocol    = get_protocol_type(proplists:get_value(<<"protocol">>, Action), State), 
    Domain      = get_server_domain(proplists:get_value(<<"server">>, Action), State), 
    Port        = get_port_value(proplists:get_value(<<"port">>, Action), State), 
    Page        = binary_to_list(proplists:get_value(<<"url">>, Action)), 
    URL         = lists:concat([Protocol, "://", Domain, ":", Port, Page]),

    Header      = [{"P3P", "CP='ALL IND DSP COR ADM CONo CUR CUSo IVAo IVDo PSA PSD TAI TELo OUR SAMo CNT COM INT NAV ONL PHY PRE PUR UNI'"}],
    HTTPOps     = [],
    Ops         = [],

    %%% Optional Defined
    Args        = proplists:get_value(<<"args">>, Action),
    Results     = proplists:get_value(<<"results">>, Action),
    ResultsType = proplists:get_value(<<"results_type">>, Action),

    case proplists:get_value(<<"type">>, Action) of
        <<"get">> ->
            LURL            = encode_uri_args( URL, Args, State#state.keystore),
            {Time, Result}  = timer:tc(httpc, request, [
                                    get, 
                                    {LURL, Header}, 
                                    HTTPOps, Ops, State#state.profile]),
            NewState = process_action_results(  Result, 
                                                ResultsType,
                                                Results,
                                                URL,
                                                State),
            store_action_results(ActionName, Time, NewState);

        <<"post">> ->
            Type            = "application/x-www-form-urlencoded",
            Body            = encode_post_args(Args, State#state.keystore),
            {Time, Result}  = timer:tc(httpc, request, [
                                    post, 
                                    {
                                        URL, 
                                        Header, 
                                        Type, 
                                        Body
                                    }, 
                                    HTTPOps, Ops, State#state.profile]),
            NewState = process_action_results(  Result,
                                                ResultsType,
                                                Results,
                                                URL,
                                                State),
            store_action_results(ActionName, Time, NewState);

        <<"post_json">> ->
            Type            = "application/json",
            %TODO I need to do the token replacement stuff
            % As a note of refactor I should use this same token replace
            % system in the get and post args stuff
            Body            = binary_to_list(jiffy:encode(Args)),
            ReplacedBody    = replace_tokens_in_text(Body, State#state.keystore),
            {Time, Result}  = timer:tc(httpc, request, [
                                    post, 
                                    {
                                        URL, 
                                        Header, 
                                        Type, 
                                        ReplacedBody
                                    }, 
                                    HTTPOps, Ops, State#state.profile]),
            NewState = process_action_results(  Result,
                                                ResultsType,
                                                Results,
                                                URL,
                                                State),
            store_action_results(ActionName, Time, NewState);

        <<"post_multipart">> ->
            State;

        _ ->
            % Undefined type of post
            State
    end;

execute(_Action, _State) ->
    error_arguments_incorrect_type.
