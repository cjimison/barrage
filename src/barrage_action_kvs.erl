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
-export([execute_load_kvs/3]).
-export([execute_read_random_kvs/4]).
-export([execute_read_name_kvs/4]).
-export([execute_store_key_kvs/3]).

-include("barrage_gunner.hrl").

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
execute_load_kvs(Profile, File, State) ->
    BasePath    = code:priv_dir(barrage),
    {ok, KVSRaw}= file:read_file(BasePath ++ "/" ++ list_to_binary(File)), 
    {KVS}       = jiffy:decode(KVSRaw),
    KVSStore    = dic:from_list(KVS),
    StateKVS    = dic:store(Profile, KVSStore, State#state.keystore),
    State#state{keystore = StateKVS}.

execute_read_random_kvs(Profile, KeyStorageName, ValueStorageName, State) ->
    {ok, KVS}   = dic:find(Profile, State#state.keystore),
    KVSList     = dic:to_list(KVS),
    Idx         = random:uniform(length(KVSList)),
    {Name, Val} = lists:nth(Idx, KVSList),
    NameDic     = dic:store(KeyStorageName, Name, State#state.keystore), 
    ValDic      = dic:store(ValueStorageName, Val, NameDic), 
    State#state{keystore = ValDic}.

execute_read_name_kvs(Profile, KeyName, StorageName, State) ->
    {ok, KVS}   = dic:find(Profile, State#state.keystore),
    {ok, Value} = dic:find(KeyName, KVS),
    NewDic      = dic:store(StorageName, Value, State#state.keystore),
    State#state{keystore = NewDic}.

execute_store_key_kvs(Profile, KeyName, State) ->
    {ok, KVS}   = dic:find(Profile, State#state.keystore),
    {ok, KeyVal}= dic:find(KeyName, State#state.keystore),
    NewDic      = dic:store(KeyName, KeyVal, KVS),
    KeyStore    = dic:store(Profile, NewDic, State#state.keystore),
    State#state{keystore=KeyStore}.


%%%===================================================================
%%% Internal functions
%%%===================================================================




