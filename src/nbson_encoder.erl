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
-spec encode(Data) -> Result when
    Data ::
        undefined | [{}] | nbson:map_document() | nbson:proplist_document() | [nbson:document()],
    Result :: {ok, BSON} | {error, nbson:encode_error_reason()},
    BSON :: binary().
encode(undefined) ->
    {ok, <<>>};
encode(Document) when is_map(Document), map_size(Document) == 0 ->
    {ok, ?EMPTY_DOC};
encode(Document) when is_map(Document) ->
    case encode_map(Document) of
        {error, _Reason} = Error ->
            Error;
        Encoded ->
            {ok, Encoded}
    end;
encode([{}]) ->
    {ok, ?EMPTY_DOC};
encode([{K, _V} | _Rest] = Data) when is_binary(K) ->
    case encode_proplist(Data) of
        {error, _Reason} = Error ->
            Error;
        Encoded ->
            {ok, Encoded}
    end;
encode(Data) when is_list(Data), is_map(hd(Data)) ->
    encode_map_list(Data, <<>>).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
-spec encode_label(Value) -> Result when
    Value :: integer() | binary(),
    Result :: binary().
encode_label(Label) when is_integer(Label) ->
    integer_to_binary(Label);
encode_label(Label) when is_binary(Label) ->
    Label.

-spec encode_list(Data) -> Result when
    Data :: [nbson:document()],
    Result :: binary() | {error, nbson:encode_error_reason()}.
encode_list([]) ->
    ?EMPTY_DOC;
encode_list(Documents) ->
    case foldwhile(fun list_fold_encode/2, {0, <<>>}, Documents) of
        {error, _Reason} = Error ->
            Error;
        {_, Encoded} ->
            <<?INT32(byte_size(Encoded) + 5), Encoded/binary, ?NULL>>
    end.

-spec encode_map(Data) -> Result when
    Data :: nbson:map_document(),
    Result :: binary() | {error, nbson:encode_error_reason()}.
encode_map(Document) ->
    case maps:fold(fun map_fold_encode/3, <<>>, Document) of
        {error, _Reason} = Error ->
            Error;
        Encoded ->
            <<?INT32(byte_size(Encoded) + 5), Encoded/binary, ?NULL>>
    end.

-spec encode_map_list(MapList, Acc) -> Result when
    MapList :: [nbson:map_document()],
    Acc :: binary(),
    Result :: {ok, binary()} | {error, nbson:encode_error_reason()}.
encode_map_list([], Acc) ->
    {ok, Acc};
encode_map_list([Doc | Rest], Acc) ->
    case encode_map(Doc) of
        {error, _Reason} = Error ->
            Error;
        Bin ->
            encode_map_list(Rest, <<Acc/binary, Bin/binary>>)
    end.

-spec encode_proplist(Data) -> Result when
    Data :: nbson:proplist_document(),
    Result :: binary() | {error, nbson:encode_error_reason()}.
encode_proplist(Proplist) ->
    case encode_proplist(Proplist, <<>>) of
        {error, _Reason} = Error ->
            Error;
        Encoded ->
            <<?INT32(byte_size(Encoded) + 5), Encoded/binary, ?NULL>>
    end.

-spec encode_proplist(Data, Acc) -> Result when
    Data :: list(),
    Acc :: binary(),
    Result :: binary() | {error, nbson:encode_error_reason()}.
encode_proplist([], Acc) ->
    Acc;
encode_proplist([{_Label, undefined} | Rest], Acc) ->
    encode_proplist(Rest, Acc);
encode_proplist([{Label, Value} | Rest], Acc) ->
    case encode_value(Value) of
        {error, _Reason} = Error ->
            Error;
        {Type, Payload} ->
            encode_proplist(
                Rest,
                <<Acc/binary, ?INT8(Type), ?CSTRING(encode_label(Label)), Payload/binary>>
            )
    end;
encode_proplist([Other | _Rest], _Acc) ->
    {error, {invalid_proplist_document, Other}}.

-spec encode_value(Value) -> Result when
    Value :: nbson:value(),
    Result :: {Type, binary()} | {error, nbson:encode_error_reason()},
    Type :: 1..255.
encode_value(V) when is_float(V) ->
    {?DOUBLE_TYPE, <<?DOUBLE(V)>>};
encode_value(V) when is_binary(V) ->
    {?STRING_TYPE, <<?INT32(byte_size(V) + 1), ?CSTRING(V)>>};
encode_value(V) when is_map(V) ->
    case encode(V) of
        {error, _Reason} = Error ->
            Error;
        {ok, Bson} ->
            {?EMBDOC_TYPE, Bson}
    end;
encode_value(V) when is_list(V), is_tuple(hd(V)), is_binary(element(1, hd(V))) ->
    case encode(V) of
        {error, _Reason} = Error ->
            Error;
        {ok, Bson} ->
            {?EMBDOC_TYPE, Bson}
    end;
encode_value(V) when is_list(V) ->
    case encode_list(V) of
        {error, _Reason} = Error ->
            Error;
        EncodedList ->
            {?ARRAY_TYPE, EncodedList}
    end;
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
%max_key and min_key should be the last and ordered by Type value. But gradualizer doesn't like that
encode_value(max_key) ->
    {?MAXKEY_TYPE, <<>>};
encode_value(min_key) ->
    {?MINKEY_TYPE, <<>>};
encode_value({Mega, Sec, Micro}) when is_integer(Mega), is_integer(Sec), is_integer(Micro) ->
    {?DATETIME_TYPE, <<?INT64(Mega * 1000000000 + Sec * 1000 + Micro div 1000)>>};
encode_value(null) ->
    {?NULL_TYPE, <<>>};
encode_value({regex, Pattern, Options}) ->
    case {unicode:characters_to_binary(Pattern), unicode:characters_to_binary(Options)} of
        {PBin, OBin} when is_binary(PBin) andalso is_binary(OBin) ->
            {?REGEX_TYPE, <<?CSTRING(PBin), ?CSTRING(OBin)>>};
        _NotUnicode ->
            {error, {not_unicode_regex, {Pattern, Options}}}
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
    case encode(Scope) of
        {error, _Reason} = Error ->
            Error;
        {ok, EncodedScope} ->
            Encoded = <<?INT32(byte_size(CStringCode)), CStringCode/binary, (EncodedScope)/binary>>,
            {?JSCODEWS_TYPE, <<?INT32(byte_size(Encoded) + 4), Encoded/binary>>}
    end;
encode_value(V) when is_integer(V), -16#80000000 =< V, V =< 16#7fffffff ->
    {?INT32_TYPE, <<?INT32(V)>>};
encode_value({timestamp, Inc, Time}) ->
    {?TIMESTAMP_TYPE, <<?INT32(Inc), ?INT32(Time)>>};
encode_value(V) when is_integer(V), -16#8000000000000000 =< V, V =< 16#7fffffffffffffff ->
    {?INT64_TYPE, <<?INT64(V)>>};
encode_value(V) when is_integer(V) ->
    {error, {integer_too_large, V}}.

-spec foldwhile(F, AccIn, List) -> Result when
    F :: fun((term(), AccIn) -> AccOut),
    AccIn :: term(),
    AccOut :: term(),
    List :: list(term()),
    Result :: term().
foldwhile(F, AccIn, [Hd | Tail]) when is_function(F, 2) ->
    case F(Hd, AccIn) of
        {error, _Reason} = Error ->
            Error;
        AccOut ->
            foldwhile(F, AccOut, Tail)
    end;
foldwhile(F, AccIn, []) when is_function(F, 2) ->
    AccIn.

-spec list_fold_encode(Document, {Pos, Acc}) -> Result when
    Document :: nbson:document(),
    Pos :: non_neg_integer(),
    Acc :: binary(),
    Result :: {non_neg_integer(), binary()} | {error, nbson:encode_error_reason()}.
list_fold_encode(Document, {Pos, Acc}) ->
    case encode_value(Document) of
        {error, _Reason} = Error ->
            Error;
        {Type, Payload} ->
            {Pos + 1, <<Acc/binary, ?INT8(Type), ?CSTRING(encode_label(Pos)), Payload/binary>>}
    end.

-spec map_fold_encode(Label, Value, Acc) -> Result when
    Label :: integer() | binary(),
    Value :: nbson:value(),
    Acc :: binary(),
    Result :: binary() | {error, nbson:encode_error_reason()}.
map_fold_encode(_Label, undefined, Acc) ->
    Acc;
map_fold_encode(Label, Value, Acc) ->
    case encode_value(Value) of
        {error, _Reason} = Error ->
            Error;
        {Type, Payload} ->
            <<Acc/binary, ?INT8(Type), ?CSTRING(encode_label(Label)), Payload/binary>>
    end.
