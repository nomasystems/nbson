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
%%% Based on jsone CPS style, see
%% https://github.com/sile/jsone/blob/master/src/jsone_decode.erl

-module(nbson_decode).

%%% INCLUDE FILES
-include("nbson_bson_types.hrl").

%%% EXTERNAL EXPORTS
-export([decode/1]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
decode(Bson) ->
    document(Bson, [], [{document, []}]).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
next(<<Bin/binary>>, Current, []) ->
    {[Current], Bin};
next(<<>>, Current, [{document, Docs}]) ->
    {lists:reverse([Current | Docs]), <<>>};
next(<<Bin/binary>>, Current, [{document, Docs}]) ->
    document(Bin, [], [{document, [Current | Docs]}]);
next(<<Bin/binary>>, Current, [{evalue, Type, document, Members} | Next]) ->
    evalue(Bin, Type, [{elist, Current, Members} | Next]);
next(<<Bin/binary>>, _Current, [{evalue, Type, array, Members} | Next]) ->
    evalue(Bin, Type, [{array, Members} | Next]);
next(<<Bin/binary>>, Current, [{elist, Name, Members} | Next]) ->
    elist(Bin, document, [{Name, Current} | Members], Next);
next(<<Bin/binary>>, Current, [{array, Members} | Next]) ->
    elist(Bin, array, [Current | Members], Next);
next(<<Bin/binary>>, Current, [regex | Next]) ->
    cstring(Bin, [{regex_opts, Current} | Next]);
next(<<Bin/binary>>, Current, [{regex_opts, Regex} | Next]) ->
    next(Bin, {regex, Regex, Current}, Next);
next(<<Bin/binary>>, Current, [db_pointer | Next]) ->
    pointer(Bin, [{db_pointer, Current} | Next]);
next(<<Bin/binary>>, Current, [{db_pointer, Collection} | Next]) ->
    next(Bin, {pointer, Collection, Current}, Next);
next(<<Bin/binary>>, Current, [jscode | Next]) ->
    next(Bin, {javascript, [{}], Current}, Next);
next(<<Bin/binary>>, Current, [label | Next]) ->
    next(Bin, Current, Next);
next(<<Bin/binary>>, Current, [jscodews | Next]) ->
    document(Bin, [], [{jscodews, Current} | Next]);
next(<<Bin/binary>>, Current, [{jscodews, Code} | Next]) ->
    next(Bin, {javascript, Current, Code}, Next);
next(<<Bin/binary>>, Current, Next) ->
    {error, next, Bin, Current, Next}.

document(<<?INT32(_Size), Bin/binary>>, Members, Next) ->
    elist(Bin, document, Members, Next).

elist(<<0, Bin/binary>>, document, [], Next) ->
    next(Bin, [{}], Next);
elist(<<0, Bin/binary>>, _Kind, Members, Next) ->
    next(Bin, lists:reverse(Members), Next);
elist(<<Bin/binary>>, Kind, Members, Next) ->
    elem(Bin, Kind, Members, Next).

elem(<<?INT8(Type), Bin/binary>>, Kind, Members, Next) ->
    cstring(Bin, [{evalue, Type, Kind, Members} | Next]).

evalue(<<?DOUBLE(D), Bin/binary>>, ?DOUBLE_TYPE, Next) ->
    next(Bin, D, Next);
evalue(<<?INT32(L), Bin/binary>>, ?STRING_TYPE, Next) ->
    string(Bin, L - 1, Next);
evalue(<<Bin/binary>>, ?EMBDOC_TYPE, Next) ->
    document(Bin, [], Next);
evalue(<<?INT32(_L), Bin/binary>>, ?ARRAY_TYPE, Next) ->
    array(Bin, [], Next);
evalue(<<?INT32(Size), ?INT8(SubType), Bin/binary>>, ?BIN_TYPE, Next) ->
    binary(Bin, Size, SubType, Next);
evalue(<<Bin/binary>>, ?UNDEF_TYPE, Next) ->
    next(Bin, undefined, Next);
evalue(<<?BITS96(V), Bin/binary>>, ?OBJID_TYPE, Next) ->
    next(Bin, {object_id, V}, Next);
evalue(<<?INT8(V), Bin/binary>>, ?BOOLEAN_TYPE, Next) ->
    next(Bin, V /= 0, Next);
evalue(<<?INT64(V), Bin/binary>>, ?DATETIME_TYPE, Next) ->
    DateTime = {V div 1000000000, (V div 1000) rem 1000000, (V * 1000) rem 1000000},
    next(Bin, DateTime, Next);
evalue(<<Bin/binary>>, ?NULL_TYPE, Next) ->
    next(Bin, null, Next);
evalue(<<Bin/binary>>, ?REGEX_TYPE, Next) ->
    cstring(Bin, [regex | Next]);
evalue(<<?INT32(L), Bin/binary>>, ?DBPOINTER_TYPE, Next) ->
    string(Bin, L - 1, [db_pointer | Next]);
evalue(<<?INT32(L), Bin/binary>>, ?JSCODE_TYPE, Next) ->
    string(Bin, L - 1, [jscode | Next]);
evalue(<<?INT32(L), Bin/binary>>, ?SYMBOL_TYPE, Next) ->
    string(Bin, L - 1, [label | Next]);
evalue(<<?INT32(_L), ?INT32(LCode), Bin/binary>>, ?JSCODEWS_TYPE, Next) ->
    string(Bin, LCode - 1, [jscodews | Next]);
evalue(<<?INT32(I), Bin/binary>>, ?INT32_TYPE, Next) ->
    next(Bin, I, Next);
evalue(<<?INT32(Inc), ?INT32(Time), Bin/binary>>, ?TIMESTAMP_TYPE, Next) ->
    next(Bin, {timestamp, Inc, Time}, Next);
evalue(<<?INT64(I), Bin/binary>>, ?INT64_TYPE, Next) ->
    next(Bin, I, Next);
evalue(<<Bin/binary>>, ?MAXKEY_TYPE, Next) ->
    next(Bin, max_key, Next);
evalue(<<Bin/binary>>, ?MINKEY_TYPE, Next) ->
    next(Bin, min_key, Next).

array(<<0, Bin/binary>>, Members, Next) ->
    next(Bin, Members, Next);
array(<<Bin/binary>>, Members, Next) ->
    elem(Bin, array, Members, Next).

pointer(<<?BITS96(Id), Bin/binary>>, Next) ->
    next(Bin, Id, Next).

cstring(<<Bin/binary>>, Next) ->
    Len = cstring_len(Bin, 0),
    string(Bin, Len, Next).

cstring_len(<<?NULL, _Rest/binary>>, Len) ->
    Len;
cstring_len(<<_C, Rest/binary>>, Len) ->
    cstring_len(Rest, Len + 1).

string(Base, Len, Next) ->
    <<String:Len/binary, ?NULL, Bin/binary>> = Base,
    next(Bin, String, Next).

binary(<<Base/binary>>, Size, SubType, Next) ->
    <<Part:Size/binary, Bin/binary>> = Base,
    next(Bin, {data, subtype_decode(SubType), Part}, Next).

subtype_decode(0) ->
    binary;
subtype_decode(1) ->
    function;
subtype_decode(2) ->
    binary;
subtype_decode(3) ->
    uuid;
subtype_decode(4) ->
    uuid;
subtype_decode(5) ->
    md5;
subtype_decode(6) ->
    encrypted;
subtype_decode(128) ->
    user.
