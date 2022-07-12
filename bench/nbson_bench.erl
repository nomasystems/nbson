%%% Copyright (C) 2009 Nomasystems, S.L.
%%%
%%% This file contains Original Code and/or Modifications of Original
%%% Code as defined in and that are subject to the Nomasystems Public
%%% is provided with the Original Code and Modifications, and is also
%%% available at www.nomasystems.com/license.txt.
%%%
%%% The Original Code and all software distributed under the License
%%% are distributed on an 'AS IS' basis, WITHOUT WARRANTY OF ANY KIND,
%%% EITHER EXPRESS OR IMPLIED, AND NOMASYSTEMS AND ALL CONTRIBUTORS
%%% HEREBY DISCLAIM ALL SUCH WARRANTIES, INCLUDING WITHOUT LIMITATION,
%%% ANY WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE,
%%% QUIET ENJOYMENT OR NON-INFRINGEMENT. Please see the License for
%%% the specific language governing rights and limitations under the
%%% License.
-module(nbson_bench).

-define(SEPARATOR, io:format("--------------------------------------------------------------------------------------~n")).
                                                                                                                          
%%% EXTERNAL EXPORTS
-export([bench/0, bench_decode/0, bench_encode/0, profile_decode/0, profile_encode/0]).

%%% MACROS
-define(TIMES , 10).


%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
bench() ->
    bench_decode(),
    bench_encode().

bench_decode() ->
    Times = ?TIMES,
    ?SEPARATOR,
    io:format("Decoder:~n"),
    ?SEPARATOR,
    head(),
    bench_decode("bench/data/test1.bson", Times),
    bench_decode("bench/data/test10.bson", Times),
    bench_decode("bench/data/test100.bson", Times),
    bench_decode("bench/data/test1000.bson", Times),
    bench_decode("bench/data/test10000.bson", Times),
    bench_decode("bench/data/test100000.bson", Times),
    ?SEPARATOR.
 
bench_encode() ->
    Times = ?TIMES,
    ?SEPARATOR,
    io:format("Encoder:~n"),
    ?SEPARATOR,
    head(),
    bench_encode("bench/data/test1.bson", Times),
    bench_encode("bench/data/test10.bson", Times),
    bench_encode("bench/data/test100.bson", Times),
    bench_encode("bench/data/test1000.bson", Times),
    bench_encode("bench/data/test10000.bson", Times),
    bench_encode("bench/data/test100000.bson", Times),
    ?SEPARATOR.



profile_decode() ->
    Path = "bench/test.json",
    {ok, Bin} = file:read_file(Path),
    eflambe:apply({njson, decode, [Bin]}, [{output_format, brendan_gregg}]).

profile_encode() ->
    Path = "bench/test.json",
    {ok, Bin} = file:read_file(Path),
    {ok, J, _} = njson:decode(Bin),
    eflambe:apply({njson, encode, [J]}, [{output_format, brendan_gregg}]).



%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
head() ->
    io:format("~20.. s  ~20.. s  ~20.. s  ~20.. s~n",
              ["Size (documents)", "File size (bytes)", "Nbson Time (us)", "BsonErlang Time (us)"]).
 
bench_decode(Path, Times) ->
    {ok, Bin} = file:read_file(Path),
    DocCount = doc_count(Path),
    NbsonTimeDecode = erlperf:time(fun() -> nbson:decode_all(Bin) end, Times),
    BsonErlangTimeDecode = erlperf:time(fun() -> get_docs(Bin, []) end, Times),
    io:format("~20.. s  ~20.. B  ~20.. B  ~20.. B~n",
              [DocCount,
               byte_size(Bin),
               round(NbsonTimeDecode/Times),
               round(BsonErlangTimeDecode/Times)]).

  
bench_encode(Path, Times) ->
    {ok, Bin} = file:read_file(Path),
    DocCount = doc_count(Path),
    NbsonTimeEncode = if
                    DocCount > 1 ->
                       NbsonDocs = nbson:decode_all(Bin),
                       erlperf:time(fun() -> nbson:encode_all(NbsonDocs) end, Times);
                    true ->
                        NbsonDocs = nbson:decode(Bin),
                       erlperf:time(fun() -> nbson:encode(NbsonDocs) end, Times)
                end,

    {BsonErlangDocs, _Rest2} = get_docs(Bin, []),

    BsonErlangTimeEncode = erlperf:time(fun() -> put_docs(BsonErlangDocs) end, Times),
    io:format("~20.. s  ~20.. B  ~20.. B  ~20.. B~n",
              [DocCount,
               byte_size(Bin),
               round(NbsonTimeEncode/Times),
               round(BsonErlangTimeEncode/Times)]).


% get_docs implementation extracted from https://github.com/comtihon/mongodb-erlang/blob/56c700f791601a201a9d5af7cad45b3c81258209/src/connection/mongo_protocol.erl#L113
get_docs(<<>>, Docs) ->
    {lists:reverse(Docs), <<>>};
get_docs(Bin, Docs) ->
    {Doc, Bin1} = bson_binary:get_map(Bin),
    get_docs(Bin1, [Doc | Docs]).

% Multiple documents encoding implementation taken from https://github.com/comtihon/mongodb-erlang/blob/56c700f791601a201a9d5af7cad45b3c81258209/src/connection/mongo_protocol.erl#L52
put_docs(Docs) ->
    << << <<(bson_binary:put_document(Doc))/binary>> || Doc <- Docs>>/binary >>.

doc_count(Path) ->
    [_, Filename] = string:split(Path, "/", trailing),
    [_, Filename1] = string:split(Filename, "test", trailing),
    [Number, _] = string:split(Filename1, "."),
    Number.


