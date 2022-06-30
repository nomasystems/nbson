%%% Copyright 2022 Nomasystems, S.L. http://www.nomasystems.com
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
-module(nbson_encode).

%%% INCLUDE FILES
-include("nbson_bson_types.hrl").

%%% EXTERNAL EXPORTS
-export([encode/1]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
encode(undefined) ->
    <<>>;
encode([{}]) ->
    <<?INT32(5), ?NULL>>;
encode(Document) when is_tuple(hd(Document)) ->
    Encoded = encode(Document, <<>>),
    <<?INT32(byte_size(Encoded) + 5), Encoded/binary, ?NULL>>.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
encode([], Acc) ->
    Acc;
encode([{Label, Value} | Rest], Acc) ->
    {Type, Payload} = encode_value(Value),
    encode(Rest, <<Acc/binary, ?INT8(Type), ?CSTRING(encode_label(Label)), Payload/binary>>).

encode_value(V) when is_float(V) ->
    {?DOUBLE_TYPE, <<?DOUBLE(V)>>};
encode_value(V) when is_binary(V) ->
    {?STRING_TYPE, <<?INT32(byte_size(V) + 1), ?CSTRING(V)>>};
encode_value(V) when is_tuple(hd(V)), is_binary(element(1, (hd(V)))) ->
    {?EMBDOC_TYPE, encode(V)};
encode_value([{}]) ->
    {?EMBDOC_TYPE, encode([{}])};
encode_value([]) ->
    {?ARRAY_TYPE, encode([{}])};
encode_value(V) when is_list(V) ->
    {?ARRAY_TYPE, encode(lists:zip(lists:seq(0, length(V) - 1), V))};
encode_value({data, binary, Data}) when is_binary(Data) ->
    {?BIN_TYPE, <<?INT32(byte_size(Data)), ?INT8(0), Data/binary>>};
encode_value({data, function, Data}) when is_binary(Data) ->
    {?BIN_TYPE, <<?INT32(byte_size(Data)), ?INT8(1), Data/binary>>};
encode_value({data, uuid, Data}) when is_binary(Data) ->
    {?BIN_TYPE, <<?INT32(byte_size(Data)), ?INT8(3), Data/binary>>};
encode_value({data, md5, Data}) when is_binary(Data) ->
    {?BIN_TYPE, <<?INT32(byte_size(Data)), ?INT8(5), Data/binary>>};
encode_value({data, user, Data}) when is_binary(Data) ->
    {?BIN_TYPE, <<?INT32(byte_size(Data)), ?INT8(128), Data/binary>>};
encode_value(undefined) ->
    {?UNDEF_TYPE, <<>>};
encode_value({object_id, <<_:96>> = Id}) ->
    {?OBJID_TYPE, Id};
encode_value(false) ->
    {?BOOLEAN_TYPE, <<?INT8(0)>>};
encode_value(true) ->
    {?BOOLEAN_TYPE, <<?INT8(1)>>};
encode_value({Mega, Sec, Micro}) when is_integer(Mega), is_integer(Sec), is_integer(Micro) ->
    {?DATETIME_TYPE, <<?INT64(Mega * 1000000000 + Sec * 1000 + Micro div 1000)>>};
encode_value(null) ->
    {?NULL_TYPE, <<>>};
encode_value({regex, Pattern, Options}) ->
    {?REGEX_TYPE, <<
        ?CSTRING(unicode:characters_to_binary(Pattern)),
        ?CSTRING(unicode:characters_to_binary(Options))
    >>};
encode_value({pointer, Collection, <<_:96>> = Id}) ->
    {?DBPOINTER_TYPE, <<?INT32(byte_size(Collection) + 1), ?CSTRING(Collection), Id/binary>>};
encode_value({javascript, [{}], Code}) when is_binary(Code) ->
    {?JSCODE_TYPE, <<?INT32(byte_size(Code) + 1), ?CSTRING(Code)>>};
encode_value(V) when is_atom(V), V =/= min_key, V =/= max_key ->
    VBin = atom_to_binary(V, utf8),
    {?SYMBOL_TYPE, <<?INT32(byte_size(VBin) + 1), ?CSTRING(VBin)>>};
encode_value({javascript, Scope, Code}) when is_tuple(hd(Scope)), is_binary(Code) ->
    CStringCode = <<?CSTRING(Code)>>,
    Encoded = <<?INT32(byte_size(CStringCode)), CStringCode/binary, (encode(Scope))/binary>>,
    {?JSCODEWS_TYPE, <<?INT32(byte_size(Encoded) + 4), Encoded/binary>>};
encode_value(V) when is_integer(V), -16#80000000 =< V, V =< 16#7fffffff ->
    {?INT32_TYPE, <<?INT32(V)>>};
encode_value({timestamp, Inc, Time}) ->
    {?TIMESTAMP_TYPE, <<?INT32(Inc), ?INT32(Time)>>};
encode_value(V) when is_integer(V), -16#8000000000000000 =< V, V =< 16#7fffffffffffffff ->
    {?INT64_TYPE, <<?INT64(V)>>};
encode_value(V) when is_integer(V) ->
    erlang:error(integer_too_large, [V]);
encode_value(max_key) ->
    {?MAXKEY_TYPE, <<>>};
encode_value(min_key) ->
    {?MINKEY_TYPE, <<>>}.

encode_label(Label) when is_integer(Label) ->
    list_to_binary(integer_to_list(Label));
encode_label(Label) when is_atom(Label) ->
    erlang:atom_to_binary(Label, utf8);
encode_label(Label) when is_binary(Label) ->
    Label.
