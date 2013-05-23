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
%%% @end
%%% Created : 2013-04-23 15:23:23.698042
%%%-------------------------------------------------------------------
-module(barrage_action_kvs).

%% API
-export([load/3]).
-export([load_array/3]).
-export([read_random/4]).
-export([read_name/4]).
-export([store_key/4]).

-include("barrage_gunner.hrl").

-define (is_dict (D), is_tuple (D) andalso element (1, D) =:= dict).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec 
%% @end
%%--------------------------------------------------------------------
load(Profile, File, State) ->
    BasePath    = code:priv_dir(barrage),
    LFile       = binary_to_list(File),
    Path        = BasePath ++ "/" ++ LFile,
    {ok, KVSRaw}= file:read_file(Path), 
    {KVS}       = jiffy:decode(KVSRaw),
    KVSStore    = dict:from_list(KVS),
    StateKVS    = dict:store(Profile, KVSStore, State#state.keystore),
    State#state{keystore = StateKVS}.


load_array(Profile, File, State) ->
    BasePath    = code:priv_dir(barrage),
    LFile       = binary_to_list(File),
    Path        = BasePath ++ "/" ++ LFile,
    {ok, KVSRaw}= file:read_file(Path), 
    ArrayStore  = jiffy:decode(KVSRaw),
    StateKVS    = dict:store(Profile, ArrayStore, State#state.keystore),
    State#state{keystore = StateKVS}.

read_random(Profile, KeyStorageName, ValueStorageName, State) ->
    {ok, KVS}   = dict:find(Profile, State#state.keystore),
    KVSList     = dict:to_list(KVS),
    Idx         = random:uniform(length(KVSList)),
    {Name, Val} = lists:nth(Idx, KVSList),
    NameDic     = dict:store(KeyStorageName, Name, State#state.keystore), 
    ValDic      = dict:store(ValueStorageName, Val, NameDic), 
    State#state{keystore = ValDic}.

read_name(Profile, KeyName, StorageName, State) ->
    {ok, KVS}   = dict:find(Profile, State#state.keystore),
    case is_list(KVS) of
        true ->
            Value       = proplists:get_value(KeyName, KVS),
            NewDic      = dict:store(StorageName, Value, State#state.keystore),
            State#state{keystore = NewDic};
        false ->
            case ?is_dict(KVS) of
                true ->
                    {ok, Value} = dict:find(KeyName, KVS),
                    NewDic      = dict:store(StorageName, Value, State#state.keystore),
                    State#state{keystore = NewDic};
                false ->
                    {PLkvs}     = KVS,
                    Value       = proplists:get_value(KeyName, PLkvs),
                    NewDic      = dict:store(StorageName, Value, State#state.keystore),
                    State#state{keystore = NewDic}
            end
    end.

store_key(VarName, Profile, KeyName, State) ->
    {ok, KVS}   = dict:find(Profile, State#state.keystore),
    {ok, KeyVal}= dict:find(VarName, State#state.keystore),
    NewDic      = dict:store(KeyName, KeyVal, KVS),
    KeyStore    = dict:store(Profile, NewDic, State#state.keystore),
    State#state{keystore=KeyStore}.

%%%===================================================================
%%% Internal functions
%%%===================================================================




