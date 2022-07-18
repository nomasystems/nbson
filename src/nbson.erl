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
-type nbson_key() :: binary().
-type nbson_value() :: any().
-type document() :: #{} | proplists:proplist().
-type document_path() :: [nbson_key()].

-export_type([document/0]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec encode(Data) -> Result when
    Data :: undefined | document() | list(document()),
    Result :: binary() | list(binary()).
encode(Data) ->
    nbson_encoder:encode(Data).

-spec decode(Data) -> Result when
    Data :: binary(),
    Result :: list(document()).
decode(Data) ->
    nbson_decoder:decode(Data).

-spec get(Path, Document) -> Result when
    Path :: document_path() | nbson_key(),
    Document :: document(),
    Result :: nbson_value().
get([], Document) ->
    Document;
get([Label | Rest], Document) when is_binary(Label) ->
    case maps:get(Label, Document, nbson_get_not_found) of
        nbson_get_not_found ->
            undefined;
        Value ->
            get(Rest, Value)
    end;
get(Label, Document) when is_binary(Label) ->
    get([Label], Document).
