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
-export([encode/1, decode/1]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec encode(Data) -> Result when
    Data :: document(),
    Result :: binary() | list(binary()).
encode(undefined) ->
    <<>>;
encode(Data) when is_map(Data) ->
    nbson_encode:encode(Data);
encode(Data) when is_list(Data), is_map(hd(Data)) ->
    <<<<<<(nbson_encode:encode(Doc))/binary>> || Doc <- Data>>/binary>>.

-spec decode(Data) -> Result when
    Data :: binary(),
    Result :: document() | list(document()).
decode(Data) ->
    decode_all(Data, []).

decode_all(Data, Acc) ->
    case nbson_decode:decode(Data) of
        {Doc, <<>>} when Acc == [] ->
            Doc;
        {Doc, <<>>} ->
            lists:reverse([Doc | Acc]);
        {Doc, Rest} ->
            decode_all(Rest, [Doc | Acc])
    end.
