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
-module(nbson).

%%% EXTERNAL EXPORTS
-export([encode/1, decode/1, get/2]).

%%% TYPES
-type document() :: #{key() => value()} | [{key(), value()}].
-type document_path() :: [key()].
-type key() :: binary().
-type value() :: any().

%%% EXPORT TYPES
-export_type([
    document/0,
    document_path/0,
    key/0,
    value/0
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec encode(Data) -> Result when
    Data :: undefined | document() | [document()],
    Result :: {ok, BSON} | {error, term()},
    BSON :: binary() | [binary()].
encode(Data) ->
    nbson_encoder:encode(Data).

-spec decode(Data) -> Result when
    Data :: binary(),
    Result :: {ok, [document()]} | {error, term()}.
decode(Data) ->
    nbson_decoder:decode(Data).

-spec get(Path, Document) -> Result when
    Path :: document_path() | key(),
    Document :: document(),
    Result :: value().
get(Path, Document) ->
    do_get(Path, Document).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
do_get([], Document) ->
    Document;
do_get([Label | Rest], Document) when is_map(Document) ->
    case maps:get(Label, Document, nbson_get_not_found) of
        nbson_get_not_found ->
            undefined;
        Value ->
            do_get(Rest, Value)
    end;
do_get([Label | Rest], Document) when is_list(Document) ->
    case proplists:get_value(Label, Document, nbson_get_not_found) of
        nbson_get_not_found ->
            undefined;
        Value ->
            do_get(Rest, Value)
    end;
do_get(Label, Document) when is_binary(Label) ->
    do_get([Label], Document).
