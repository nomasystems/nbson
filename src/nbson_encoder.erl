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
-module(nbson_encoder).

%%% INCLUDE FILES
-include("nbson_bson_types.hrl").

%%% EXTERNAL EXPORTS
-export([encode/1]).

%%% MACROS
-define(EMPTY_DOC, <<?INT32(5), ?NULL>>).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
encode(undefined) ->
    <<>>;
encode(Document) when is_map(Document), map_size(Document) == 0 ->
    ?EMPTY_DOC;
encode(Document) when is_map(Document) ->
    encode_map(Document);
encode([{}]) ->
    ?EMPTY_DOC;
encode(Data) when is_list(Data), is_tuple(hd(Data)), size(hd(Data)) == 2 ->
    encode_proplist(Data);
encode(Data) when is_list(Data), is_map(hd(Data)) ->
    <<<<<<(encode_map(Doc))/binary>> || Doc <- Data>>/binary>>.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
encode_list([]) ->
    ?EMPTY_DOC;
encode_list(Documents) ->
    {_, Encoded} = lists:foldl(fun list_fold_encode/2, {0, <<>>}, Documents),
    <<?INT32(byte_size(Encoded) + 5), Encoded/binary, ?NULL>>.

list_fold_encode(Document, {Pos, Acc}) ->
    {Type, Payload} = encode_value(Document),
    {Pos + 1, <<Acc/binary, ?INT8(Type), ?CSTRING(encode_label(Pos)), Payload/binary>>}.

encode_map(Document) ->
    Encoded = maps:fold(fun map_fold_encode/3, <<>>, Document),
    <<?INT32(byte_size(Encoded) + 5), Encoded/binary, ?NULL>>.

map_fold_encode(_Label, undefined, Acc) ->
    Acc;
map_fold_encode(Label, Value, Acc) ->
    {Type, Payload} = encode_value(Value),
    <<Acc/binary, ?INT8(Type), ?CSTRING(encode_label(Label)), Payload/binary>>.

encode_proplist(Proplist) ->
    Encoded = encode_proplist(Proplist, <<>>),
    <<?INT32(byte_size(Encoded) + 5), Encoded/binary, ?NULL>>.

encode_proplist([], Acc) ->
    Acc;
encode_proplist([{_Label, undefined} | Rest], Acc) ->
    encode_proplist(Rest, Acc);
encode_proplist([{Label, Value} | Rest], Acc) ->
    {Type, Payload} = encode_value(Value),
    encode_proplist(
        Rest,
        <<Acc/binary, ?INT8(Type), ?CSTRING(encode_label(Label)), Payload/binary>>
    ).

encode_value(V) when is_float(V) ->
    {?DOUBLE_TYPE, <<?DOUBLE(V)>>};
encode_value(V) when is_binary(V) ->
    {?STRING_TYPE, <<?INT32(byte_size(V) + 1), ?CSTRING(V)>>};
encode_value(V) when is_map(V) ->
    {?EMBDOC_TYPE, encode(V)};
encode_value(V) when is_list(V), is_tuple(hd(V)), is_binary(element(1, hd(V))) ->
    {?EMBDOC_TYPE, encode(V)};
encode_value(V) when is_list(V) ->
    {?ARRAY_TYPE, encode_list(V)};
encode_value({data, binary, Data}) when is_binary(Data) ->
    {?BIN_TYPE, <<?INT32(byte_size(Data)), ?INT8(0), Data/binary>>};
encode_value({data, function, Data}) when is_binary(Data) ->
    {?BIN_TYPE, <<?INT32(byte_size(Data)), ?INT8(1), Data/binary>>};
encode_value({data, uuid, Data}) when is_binary(Data) ->
    {?BIN_TYPE, <<?INT32(byte_size(Data)), ?INT8(4), Data/binary>>};
encode_value({data, md5, Data}) when is_binary(Data) ->
    {?BIN_TYPE, <<?INT32(byte_size(Data)), ?INT8(5), Data/binary>>};
encode_value({data, encrypted, Data}) when is_binary(Data) ->
    {?BIN_TYPE, <<?INT32(byte_size(Data)), ?INT8(6), Data/binary>>};
encode_value({data, compressed, Data}) when is_binary(Data) ->
    {?BIN_TYPE, <<?INT32(byte_size(Data)), ?INT8(7), Data/binary>>};
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
    case {unicode:characters_to_binary(Pattern), unicode:characters_to_binary(Options)} of
        {PBin, OBin} when is_binary(PBin) andalso is_binary(OBin) ->
            {?REGEX_TYPE, <<?CSTRING(PBin), ?CSTRING(OBin)>>};
        _false ->
            erlang:throw(
                {badarg, {Pattern, Options}, [
                    {error_info, #{
                        module => nbson_encoder, function => encode_value, cause => not_unicode
                    }}
                ]}
            )
    end;
encode_value({pointer, Collection, <<_:96>> = Id}) ->
    {?DBPOINTER_TYPE, <<?INT32(byte_size(Collection) + 1), ?CSTRING(Collection), Id/binary>>};
encode_value({javascript, Map, Code}) when is_map(Map), map_size(Map) == 0, is_binary(Code) ->
    {?JSCODE_TYPE, <<?INT32(byte_size(Code) + 1), ?CSTRING(Code)>>};
encode_value(V) when is_atom(V), V =/= min_key, V =/= max_key ->
    VBin = atom_to_binary(V, utf8),
    {?SYMBOL_TYPE, <<?INT32(byte_size(VBin) + 1), ?CSTRING(VBin)>>};
encode_value({javascript, Scope, Code}) when is_map(Scope), is_binary(Code) ->
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
    erlang:throw(
        {badarg, V, [
            {error_info, #{
                module => nbson_encoder, function => encode_value, cause => integer_too_large
            }}
        ]}
    );
encode_value(max_key) ->
    {?MAXKEY_TYPE, <<>>};
encode_value(min_key) ->
    {?MINKEY_TYPE, <<>>}.

encode_label(Label) when is_integer(Label) ->
    integer_to_binary(Label);
encode_label(Label) when is_binary(Label) ->
    Label.
