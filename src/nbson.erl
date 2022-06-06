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

%%% INCLUDES
-include("nbson.hrl").

%%% EXTERNAL EXPORTS
-export([
    encode/1,
    decode/1
]).

-export([
    lookup/2,
    at/2
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec at(Path, Document) -> Result when
    Path :: document_path(),
    Document :: document(),
    Result :: nbson_value().
at(Path, Document) ->
    case lookup(Path, Document) of
        undefined ->
            erlang:error(missing_field);
        Value ->
            Value
    end.

-spec encode(Data) -> Result when
    Data :: document(),
    Result :: binary().
encode(Data) ->
    nbson_encode:encode(Data).

-spec decode(Data) -> Result when
    Data :: binary(),
    Result :: {list(document()), binary()}.
decode(Data) ->
    nbson_decode:decode(Data).

-spec lookup(Path, Document) -> Result when
    Path :: document_path() | nbson_key(),
    Document :: document(),
    Result :: nbson_value().
lookup([], Document) ->
    Document;
lookup([Label | Rest], Document) when is_binary(Label) ->
    case lists:keyfind(Label, 1, Document) of
        {_, Value} ->
            lookup(Rest, Value);
        false ->
            undefined
    end;
lookup(Label, Document) when is_binary(Label) ->
    lookup([Label], Document).
