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
-module(nbson_decoder).
-compile([{inline, [elist/4, evalue/3, subtype_decode/1]}]).
%-compile([bin_opt_info]).

%%% INCLUDE FILES
-include("nbson_bson_types.hrl").

%%% EXTERNAL EXPORTS
-export([decode/1]).

%%% MACROS
-define(EDOCUMENT, 0).
-define(EARRAY, 1).

%%% TYPES
-type context() :: ?EDOCUMENT | ?EARRAY.
-type kind() ::
    ?DOUBLE_TYPE
    | ?STRING_TYPE
    | ?EMBDOC_TYPE
    | ?ARRAY_TYPE
    | ?BIN_TYPE
    | ?UNDEF_TYPE
    | ?OBJID_TYPE
    | ?BOOLEAN_TYPE
    | ?DATETIME_TYPE
    | ?NULL_TYPE
    | ?REGEX_TYPE
    | ?DBPOINTER_TYPE
    | ?JSCODE_TYPE
    | ?SYMBOL_TYPE
    | ?JSCODEWS_TYPE
    | ?INT32_TYPE
    | ?TIMESTAMP_TYPE
    | ?INT64_TYPE
    | ?MAXKEY_TYPE
    | ?MINKEY_TYPE.

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec decode(Data) -> Result when
    Data :: binary(),
    Result :: {ok, [nbson:document()]} | {error, term()}.
decode(<<>>) ->
    {ok, []};
decode(Bin) when is_binary(Bin) ->
    case decode_docs(Bin, []) of
        {error, _Reason} = Error ->
            Error;
        Decoded ->
            {ok, Decoded}
    end.

-spec decode_docs(Data, Acc) -> Result when
    Data :: binary(),
    Acc :: [nbson:document()],
    Result :: [nbson:document()] | {error, term()}.
decode_docs(<<>>, Acc) ->
    lists:reverse(Acc);
decode_docs(<<?INT32(Size), _Rest/binary>> = Data, Acc) ->
    decode_docs(Data, Size, Acc).

-spec decode_docs(Data, Size, Acc) -> Result when
    Data :: binary(),
    Size :: integer(),
    Acc :: [nbson:document()],
    Result :: [nbson:document()] | {error, term()}.
decode_docs(<<Bin/binary>>, Size, Acc) ->
    case Bin of
        <<Next:Size/binary, Rest/binary>> ->
            case document(Next, #{}, [document]) of
                {error, _Reason} = Error ->
                    Error;
                Document ->
                    decode_docs(Rest, [Document | Acc])
            end;
        _Other ->
            {error, {invalid_bson, lists:reverse(Acc)}}
    end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
-spec next(Data, Current, Next) -> Result when
    Data :: binary(),
    Current :: term(),
    Next :: [term()],
    Result :: nbson:document() | {error, term()}.
next(<<>>, Current, [document]) ->
    Current;
next(<<Bin/binary>>, Current, [evalue, Type, ?EDOCUMENT, Elements | Next]) ->
    evalue(Bin, Type, [elist, Current, Elements | Next]);
next(<<Bin/binary>>, _Current, [evalue, Type, ?EARRAY, Elements | Next]) ->
    evalue(Bin, Type, [array, Elements | Next]);
next(<<Bin/binary>>, Current, [elist, Name, Elements | Next]) ->
    elist(Bin, ?EDOCUMENT, maps:put(Name, Current, Elements), Next);
next(<<Bin/binary>>, Current, [array, Elements | Next]) ->
    elist(Bin, ?EARRAY, [Current | Elements], Next);
next(<<Bin/binary>>, Current, [regex | Next]) ->
    cstring(Bin, [regex_opts, Current | Next]);
next(<<Bin/binary>>, Current, [regex_opts, Regex | Next]) ->
    next(Bin, {regex, Regex, Current}, Next);
next(<<Bin/binary>>, Current, [db_pointer | Next]) ->
    pointer(Bin, [pointer, Current | Next]);
next(<<Bin/binary>>, Current, [pointer, Collection | Next]) ->
    next(Bin, {pointer, Collection, Current}, Next);
next(<<Bin/binary>>, Current, [jscode | Next]) ->
    next(Bin, {javascript, #{}, Current}, Next);
next(<<Bin/binary>>, Current, [label | Next]) ->
    next(Bin, binary_to_atom(Current, utf8), Next);
next(<<Bin/binary>>, Current, [jscodews | Next]) ->
    document(Bin, #{}, [javascript, Current | Next]);
next(<<Bin/binary>>, Current, [javascript, Code | Next]) ->
    next(Bin, {javascript, Current, Code}, Next);
next(<<_Bin/binary>>, Current, _Next) ->
    {error, {invalid_bson, Current}}.

-spec document(Data, Elements, Next) -> Result when
    Data :: binary(),
    Elements :: map(),
    Next :: [term()],
    Result :: nbson:document() | {error, term()}.
document(<<?INT32(_Size), Bin/binary>>, Elements, Next) ->
    elist(Bin, ?EDOCUMENT, Elements, Next).

-spec elist(Data, Context, Elements, Next) -> Result when
    Data :: binary(),
    Context :: context(),
    Elements :: map() | [term()],
    Next :: [term()],
    Result :: nbson:document() | {error, term()}.
elist(<<0, Bin/binary>>, Context, Elements, Next) ->
    case Context of
        ?EARRAY when is_list(Elements) ->
            next(Bin, lists:reverse(Elements), Next);
        ?EDOCUMENT ->
            next(Bin, Elements, Next)
    end;
elist(<<Bin/binary>>, Context, Elements, Next) ->
    elem(Bin, Context, Elements, Next).

-spec elem(Data, Context, Elements, Next) -> Result when
    Data :: binary(),
    Context :: context(),
    Elements :: map() | [term()],
    Next :: [term()],
    Result :: nbson:document() | {error, term()}.
elem(<<?INT8(Type), Bin/binary>>, Context, Elements, Next) ->
    cstring(Bin, [evalue, Type, Context, Elements | Next]).

-spec evalue(Data, Kind, Next) -> Result when
    Data :: binary(),
    Kind :: kind(),
    Next :: [term()],
    Result :: nbson:document() | {error, term()}.
evalue(<<Bin/binary>>, Type, Next) ->
    case Type of
        ?DOUBLE_TYPE ->
            <<?DOUBLE(D), Rest/binary>> = Bin,
            next(Rest, D, Next);
        ?STRING_TYPE ->
            <<?INT32(L), Rest/binary>> = Bin,
            string(Rest, L - 1, Next);
        ?EMBDOC_TYPE ->
            document(Bin, #{}, Next);
        ?ARRAY_TYPE ->
            <<?INT32(_L), Rest/binary>> = Bin,
            array(Rest, [], Next);
        ?BIN_TYPE ->
            <<?INT32(Size), ?INT8(SubType), Rest/binary>> = Bin,
            binary(Rest, Size, SubType, Next);
        ?UNDEF_TYPE ->
            next(Bin, undefined, Next);
        ?OBJID_TYPE ->
            <<?BITS96(V), Rest/binary>> = Bin,
            next(Rest, {object_id, V}, Next);
        ?BOOLEAN_TYPE ->
            <<?INT8(V), Rest/binary>> = Bin,
            next(Rest, V /= 0, Next);
        ?DATETIME_TYPE ->
            <<?INT64(V), Rest/binary>> = Bin,
            DateTime = {V div 1000000000, (V div 1000) rem 1000000, (V * 1000) rem 1000000},
            next(Rest, DateTime, Next);
        ?NULL_TYPE ->
            next(Bin, null, Next);
        ?REGEX_TYPE ->
            cstring(Bin, [regex | Next]);
        ?DBPOINTER_TYPE ->
            <<?INT32(L), Rest/binary>> = Bin,
            string(Rest, L - 1, [db_pointer | Next]);
        ?JSCODE_TYPE ->
            <<?INT32(L), Rest/binary>> = Bin,
            string(Rest, L - 1, [jscode | Next]);
        ?SYMBOL_TYPE ->
            <<?INT32(L), Rest/binary>> = Bin,
            string(Rest, L - 1, [label | Next]);
        ?JSCODEWS_TYPE ->
            <<?INT32(_L), ?INT32(LCode), Rest/binary>> = Bin,
            string(Rest, LCode - 1, [jscodews | Next]);
        ?INT32_TYPE ->
            <<?INT32(I), Rest/binary>> = Bin,
            next(Rest, I, Next);
        ?TIMESTAMP_TYPE ->
            <<?INT32(Inc), ?INT32(Time), Rest/binary>> = Bin,
            next(Rest, {timestamp, Inc, Time}, Next);
        ?INT64_TYPE ->
            <<?INT64(I), Rest/binary>> = Bin,
            next(Rest, I, Next);
        ?MAXKEY_TYPE ->
            next(Bin, max_key, Next);
        ?MINKEY_TYPE ->
            next(Bin, min_key, Next)
    end.

-spec array(Data, Elements, Next) -> Result when
    Data :: binary(),
    Elements :: map() | [term()],
    Next :: [term()],
    Result :: nbson:document() | {error, term()}.
array(<<0, Bin/binary>>, Elements, Next) ->
    next(Bin, Elements, Next);
array(<<Bin/binary>>, Elements, Next) ->
    elem(Bin, ?EARRAY, Elements, Next).

pointer(<<?BITS96(Id), Bin/binary>>, Next) ->
    next(Bin, Id, Next).

-spec cstring(Data, Next) -> Result when
    Data :: binary(),
    Next :: [term()],
    Result :: nbson:document() | {error, term()}.
cstring(<<Bin/binary>>, Next) ->
    Len = cstring_len(Bin, 0),
    string(Bin, Len, Next).

-spec cstring_len(Data, Len) -> Result when
    Data :: binary(),
    Len :: non_neg_integer(),
    Result :: non_neg_integer().
cstring_len(<<?NULL, _Rest/binary>>, Len) ->
    Len;
cstring_len(<<_C, Rest/binary>>, Len) ->
    cstring_len(Rest, Len + 1).

-spec string(Data, Len, Next) -> Result when
    Data :: binary(),
    Len :: integer(),
    Next :: [term()],
    Result :: nbson:document() | {error, term()}.
string(Base, Len, Next) ->
    <<String:Len/binary, ?NULL, Bin/binary>> = Base,
    next(Bin, String, Next).

-spec binary(Data, Size, SubType, Next) -> Result when
    Data :: binary(),
    Size :: integer(),
    SubType :: non_neg_integer(),
    Next :: [term()],
    Result :: nbson:document() | {error, term()}.
binary(<<Base/binary>>, Size, SubType, Next) ->
    <<Part:Size/binary, Bin/binary>> = Base,
    case subtype_decode(SubType) of
        {error, _} = Error ->
            Error;
        Decoded ->
            next(Bin, {data, Decoded, Part}, Next)
    end.

-spec subtype_decode(SubTypeInt) -> SubType | {error, invalid_subtype} when
    SubTypeInt :: non_neg_integer(),
    SubType ::
        binary | function | binary_old | uuid_old | uuid | md5 | encrypted | compressed | user.
subtype_decode(0) ->
    binary;
subtype_decode(1) ->
    function;
subtype_decode(2) ->
    binary_old;
subtype_decode(3) ->
    uuid_old;
subtype_decode(4) ->
    uuid;
subtype_decode(5) ->
    md5;
subtype_decode(6) ->
    encrypted;
subtype_decode(7) ->
    compressed;
subtype_decode(128) ->
    user;
subtype_decode(_) ->
    {error, invalid_subtype}.
