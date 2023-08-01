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
-module(nbson_monad_errors_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("stdlib/include/assert.hrl").

%%%-----------------------------------------------------------------------------
%%% SUITE EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [
        decode_errors
    ].

%%%-----------------------------------------------------------------------------
%%% INIT SUITE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_suite(Conf) ->
    nct_util:setup_suite(Conf).

%%%-----------------------------------------------------------------------------
%%% END SUITE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_suite(Conf) ->
    nct_util:teardown_suite(Conf).

%%%-----------------------------------------------------------------------------
%%% INIT CASE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_testcase(Case, Conf) ->
    ct:print("Starting test case ~p", [Case]),
    nct_util:init_traces(Case),
    Conf.

%%%-----------------------------------------------------------------------------
%%% END CASE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_testcase(Case, Conf) ->
    nct_util:end_traces(Case),
    ct:print("Test case ~p completed", [Case]),
    Conf.

%%%-----------------------------------------------------------------------------
%%% TEST CASES
%%%-----------------------------------------------------------------------------
%        <<46, 0, 0, 0, 4, 97, 114, 114, 0, 36, 0, 0, 0, 16, 48, 0, 1, 0, 0, 0, 2, 49, 0, 4, 0, 0, 0,
%            116, 119, 111, 0, 2, 50, 0, 6, 0, 0, 0, 116, 104, 114, 101, 101, 0, 0, 0>>,

decode_errors() ->
    [{userdata, [{doc, "Tests errors on BSON decoder API."}]}].
decode_errors(_Config) ->
    BaseBin =
        <<46, 0, 0, 0, 4, 97, 114, 114, 0, 36, 0, 0, 0, 16, 48, 0, 1, 0, 0, 0, 2, 49, 0, 4, 0, 0, 0,
            116, 119, 111, 0, 2, 50, 0, 6, 0, 0, 0, 116, 104, 114, 101, 101, 0, 0, 0>>,
    BaseMap = #{<<"arr">> => [1, <<"two">>, <<"three">>]},
    ?assertEqual([BaseMap], nbson:decode(BaseBin)),

    BaseBin1 =
        <<0, 0, 0, 4, 97, 114, 114, 0, 36, 0, 0, 0, 16, 48, 0, 1, 0, 0, 0, 2, 49, 0, 4, 0, 0, 0,
            116, 119, 111, 0, 2, 50, 0, 6, 0, 0, 0, 116, 104, 114, 101, 101, 0, 0, 0>>,

    {error,
        {bson, #{cause := invalid_bson, function := decode, module := nbson_decoder, data := Data1}}} = nbson:decode(
        BaseBin1
    ),
    ct:print("BaseBin1: ~p~n", [Data1]),

    BaseBin2 =
        <<46, 0, 0, 4, 97, 114, 114, 0, 36, 0, 0, 0, 16, 48, 0, 1, 0, 0, 0, 2, 49, 0, 4, 0, 0, 0,
            116, 119, 111, 0, 2, 50, 0, 6, 0, 0, 0, 116, 104, 114, 101, 101, 0, 0, 0>>,

    {error,
        {bson, #{cause := invalid_bson, function := decode, module := nbson_decoder, data := Data2}}} = nbson:decode(
        BaseBin2
    ),
    ct:print("BaseBin1: ~p~n", [Data2]),

    BaseBin3 =
        <<46, 0, 0, 0, 97, 114, 114, 0, 36, 0, 0, 0, 16, 48, 0, 1, 0, 0, 0, 2, 49, 0, 4, 0, 0, 0,
            116, 119, 111, 0, 2, 50, 0, 6, 0, 0, 0, 116, 104, 114, 101, 101, 0, 0, 0>>,

    {error,
        {bson, #{cause := invalid_bson, function := decode, module := nbson_decoder, data := Data3}}} = nbson:decode(
        BaseBin3
    ),
    ct:print("BaseBin1: ~p~n", [Data3]),

    BaseBin4 =
        <<46, 0, 0, 0, 4, 114, 114, 0, 36, 0, 0, 0, 16, 48, 0, 1, 0, 0, 0, 2, 49, 0, 4, 0, 0, 0,
            116, 119, 111, 0, 2, 50, 0, 6, 0, 0, 0, 116, 104, 114, 101, 101, 0, 0, 0>>,

    {error,
        {bson, #{cause := invalid_bson, function := decode, module := nbson_decoder, data := Data4}}} = nbson:decode(
        BaseBin4
    ),
    ct:print("BaseBin1: ~p~n", [Data4]),

    BaseBin5 =
        <<46, 0, 0, 0, 4, 97, 114, 114, 36, 0, 0, 0, 16, 48, 0, 1, 0, 0, 0, 2, 49, 0, 4, 0, 0, 0,
            116, 119, 111, 0, 2, 50, 0, 6, 0, 0, 0, 116, 104, 114, 101, 101, 0, 0, 0>>,

    {error,
        {bson, #{cause := invalid_bson, function := decode, module := nbson_decoder, data := Data5}}} = nbson:decode(
        BaseBin5
    ),
    ct:print("BaseBin1: ~p~n", [Data5]),

    BaseBin6 =
        <<46, 0, 0, 0, 4, 97, 114, 114, 0, 0, 0, 0, 16, 48, 0, 1, 0, 0, 0, 2, 49, 0, 4, 0, 0, 0,
            116, 119, 111, 0, 2, 50, 0, 6, 0, 0, 0, 116, 104, 114, 101, 101, 0, 0, 0>>,

    {error,
        {bson, #{cause := invalid_bson, function := decode, module := nbson_decoder, data := Data6}}} = nbson:decode(
        BaseBin6
    ),
    ct:print("BaseBin1: ~p~n", [Data6]),

    BaseBin7 =
        <<46, 0, 0, 0, 4, 97, 114, 114, 0, 36, 0, 0, 0>>,

    {error,
        {bson, #{cause := invalid_bson, function := decode, module := nbson_decoder, data := Data7}}} = nbson:decode(
        BaseBin7
    ),
    ct:print("BaseBin1: ~p~n", [Data7]),

    ok.
