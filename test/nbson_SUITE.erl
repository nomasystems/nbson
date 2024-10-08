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
-module(nbson_SUITE).

-compile([export_all, nowarn_export_all]).

%%%-----------------------------------------------------------------------------
%%% SUITE EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [
        array,
        boolean,
        data_bin,
        data_compressed,
        data_encrypted,
        data_fun,
        data_md5,
        data_uuid,
        datetime,
        db_pointer,
        double,
        embdoc,
        int64,
        int64_explicit,
        js,
        js_ws,
        max_key,
        min_key,
        null,
        obj_id,
        regex,
        string,
        symbols,
        timestamp,
        undef,
        proplists,
        various
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
array() ->
    [{userdata, [{doc, "Tests array data type BSON decoder API."}]}].
array(_Config) ->
    BaseBin =
        <<46, 0, 0, 0, 4, 97, 114, 114, 0, 36, 0, 0, 0, 16, 48, 0, 1, 0, 0, 0, 2, 49, 0, 4, 0, 0, 0,
            116, 119, 111, 0, 2, 50, 0, 6, 0, 0, 0, 116, 104, 114, 101, 101, 0, 0, 0>>,
    BaseMap = #{<<"arr">> => [1, <<"two">>, <<"three">>]},
    {ok, BaseMap} = nbson:decode(BaseBin),
    {ok, BaseBin} = nbson:encode(BaseMap).

boolean() ->
    [{userdata, [{doc, "Tests boolean data type BSON decoder API."}]}].
boolean(_Config) ->
    BaseBin = <<20, 0, 0, 0, 8, 102, 97, 108, 115, 101, 0, 0, 8, 116, 114, 117, 101, 0, 1, 0>>,
    BaseMap = #{<<"true">> => true, <<"false">> => false},
    {ok, BaseMap} = nbson:decode(BaseBin),
    {ok, BaseBin} = nbson:encode(BaseMap).

data_bin() ->
    [{userdata, [{doc, "Tests data bin data type BSON decoder API."}]}].
data_bin(_Config) ->
    BaseBin =
        <<23, 0, 0, 0, 5, 98, 105, 110, 0, 8, 0, 0, 0, 0, 131, 107, 0, 4, 110, 111, 109, 97, 0>>,
    BaseMap = #{<<"bin">> => {data, binary, <<131, 107, 0, 4, 110, 111, 109, 97>>}},
    {ok, BaseMap} = nbson:decode(BaseBin),
    {ok, BaseBin} = nbson:encode(BaseMap).

data_compressed() ->
    [{userdata, [{doc, "Tests data compressed data type BSON decoder API."}]}].
data_compressed(_Config) ->
    Base = <<"this is a text">>,
    GZip = zlib:gzip(Base),
    % The 10th byte of the gzip header encodes the OS;
    % ensure that this doesn't affect the portability of the test.
    % https://www.rfc-editor.org/rfc/rfc1952#section-2.3
    OS = binary:at(GZip, 9),
    BaseBin =
        <<54, 0, 0, 0, 5, 99, 111, 109, 112, 114, 101, 115, 115, 101, 100, 0, 32, 0, 0, 0, 7, 31,
            139, 8, 0, 0, 0, 0, 0, 0, OS, 43, 201, 200, 44, 86, 0, 162, 68, 133, 146, 212, 138, 18,
            0, 33, 62, 234, 238, 14, 0, 0, 0, 0>>,
    BaseMap = #{<<"compressed">> => {data, compressed, GZip}},
    {ok, BaseMap} = nbson:decode(BaseBin),
    {ok, BaseBin} = nbson:encode(BaseMap).

data_encrypted() ->
    [{userdata, [{doc, "Tests data encrypted data type BSON decoder API."}]}].
data_encrypted(_Config) ->
    Key = <<1:256>>,
    IV = <<0:128>>,
    Text = <<"this is a text">>,
    AAD = <<>>,
    {CipherText, Tag} = crypto:crypto_one_time_aead(aes_256_gcm, Key, IV, Text, AAD, true),

    BaseBin =
        <<35, 0, 0, 0, 5, 101, 110, 99, 114, 121, 112, 116, 101, 100, 0, 14, 0, 0, 0, 6, 194, 179,
            26, 216, 90, 53, 38, 51, 98, 78, 27, 228, 65, 164, 0>>,
    BaseMap = #{<<"encrypted">> => {data, encrypted, CipherText}},
    {ok, BaseMap} = nbson:decode(BaseBin),
    {ok, BaseBin} = nbson:encode(BaseMap),
    Text = crypto:crypto_one_time_aead(aes_256_gcm, Key, IV, CipherText, AAD, Tag, false).

data_fun() ->
    [{userdata, [{doc, "Tests data fun data type BSON decoder API."}]}].
data_fun(_Config) ->
    BaseBin =
        <<66, 0, 0, 0, 5, 102, 117, 110, 0, 51, 0, 0, 0, 1, 102, 117, 110, 99, 116, 105, 111, 110,
            32, 115, 113, 117, 97, 114, 101, 40, 110, 117, 109, 98, 101, 114, 41, 32, 123, 32, 114,
            101, 116, 117, 114, 110, 32, 110, 117, 109, 98, 101, 114, 32, 42, 32, 110, 117, 109, 98,
            101, 114, 59, 32, 125, 0>>,
    BaseMap = #{
        <<"fun">> =>
            {data, function, <<"function square(number) { return number * number; }">>}
    },

    {ok, BaseMap} = nbson:decode(BaseBin),
    {ok, BaseBin} = nbson:encode(BaseMap).

data_md5() ->
    [{userdata, [{doc, "Tests data md5 data type BSON decoder API."}]}].
data_md5(_Config) ->
    Base = <<"this is a text">>,
    MD5 = crypto:hash(md5, Base),

    BaseBin =
        <<31, 0, 0, 0, 5, 109, 100, 53, 0, 16, 0, 0, 0, 5, 120, 130, 26, 5, 210, 130, 130, 46, 74,
            190, 193, 144, 192, 97, 186, 120, 0>>,
    BaseMap = #{<<"md5">> => {data, md5, MD5}},
    {ok, BaseMap} = nbson:decode(BaseBin),
    {ok, BaseBin} = nbson:encode(BaseMap).

data_uuid() ->
    [{userdata, [{doc, "Tests data uuid data type BSON decoder API."}]}].
data_uuid(_Config) ->
    BaseBin =
        <<52, 0, 0, 0, 5, 117, 117, 105, 100, 0, 36, 0, 0, 0, 4, 49, 50, 51, 101, 52, 53, 54, 55,
            45, 101, 56, 57, 98, 45, 49, 50, 100, 51, 45, 97, 52, 53, 54, 45, 52, 50, 54, 54, 49,
            52, 49, 55, 52, 48, 48, 48, 0>>,
    BaseMap = #{<<"uuid">> => {data, uuid, <<"123e4567-e89b-12d3-a456-426614174000">>}},
    {ok, BaseMap} = nbson:decode(BaseBin),
    {ok, BaseBin} = nbson:encode(BaseMap).

datetime() ->
    [{userdata, [{doc, "Tests datetime data type BSON decoder API."}]}].
datetime(_Config) ->
    BaseBin = <<17, 0, 0, 0, 9, 100, 116, 0, 223, 131, 98, 249, 127, 1, 0, 0, 0>>,
    BaseMap = #{<<"dt">> => {1649, 156457, 439000}},
    {ok, BaseMap} = nbson:decode(BaseBin),
    {ok, BaseBin} = nbson:encode(BaseMap).

db_pointer() ->
    [{userdata, [{doc, "Tests db_pointer data type BSON decoder API."}]}].
db_pointer(_Config) ->
    BaseBin =
        <<29, 0, 0, 0, 12, 112, 116, 114, 0, 3, 0, 0, 0, 110, 115, 0, 98, 80, 61, 22, 65, 21, 149,
            194, 153, 178, 194, 34, 0>>,
    BaseMap = #{
        <<"ptr">> =>
            {pointer, <<"ns">>, <<98, 80, 61, 22, 65, 21, 149, 194, 153, 178, 194, 34>>}
    },
    {ok, BaseMap} = nbson:decode(BaseBin),
    {ok, BaseBin} = nbson:encode(BaseMap).

double() ->
    [{userdata, [{doc, "Tests double data type BSON decoder API."}]}].
double(_Config) ->
    BaseBin = <<18, 0, 0, 0, 1, 111, 110, 101, 0, 0, 0, 0, 0, 0, 0, 240, 63, 0>>,
    BaseMap = #{<<"one">> => 1.0},
    {ok, BaseMap} = nbson:decode(BaseBin),
    {ok, BaseBin} = nbson:encode(BaseMap).

embdoc() ->
    [{userdata, [{doc, "Tests embdoc data type BSON decoder API."}]}].
embdoc(_Config) ->
    BaseBin =
        <<28, 0, 0, 0, 3, 100, 111, 99, 0, 18, 0, 0, 0, 2, 111, 110, 101, 0, 4, 0, 0, 0, 111, 110,
            101, 0, 0, 0>>,
    BaseMap = #{<<"doc">> => #{<<"one">> => <<"one">>}},
    {ok, BaseMap} = nbson:decode(BaseBin),
    {ok, BaseBin} = nbson:encode(BaseMap).

int64() ->
    [{userdata, [{doc, "Tests int64 data type BSON decoder API."}]}].
int64(_Config) ->
    BaseBin = <<20, 0, 0, 0, 18, 105, 110, 116, 54, 52, 0, 188, 104, 151, 147, 227, 13, 1, 23, 0>>,
    BaseMap = #{<<"int64">> => 1657621408933963964},
    {ok, BaseMap} = nbson:decode(BaseBin),
    {ok, BaseBin} = nbson:encode(BaseMap).

int64_explicit() ->
    [{userdata, [{doc, "Tests explicit int64 data type BSON decoder API."}]}].
int64_explicit(_Config) ->
    BaseBin = <<20, 0, 0, 0, 18, 105, 110, 116, 54, 52, 0, 64, 0, 0, 0, 0, 0, 0, 0, 0>>,
    BaseMap = #{<<"int64">> => {long, 64}},
    {ok, BaseBin} = nbson:encode(BaseMap).

js() ->
    [{userdata, [{doc, "Tests javascript data type BSON decoder API."}]}].
js(_Config) ->
    BaseBin =
        <<41, 0, 0, 0, 13, 99, 111, 100, 101, 0, 26, 0, 0, 0, 102, 117, 110, 99, 116, 105, 111, 110,
            40, 120, 41, 32, 123, 32, 114, 101, 116, 117, 114, 110, 32, 120, 59, 32, 125, 0, 0>>,
    BaseMap = #{<<"code">> => {javascript, #{}, <<"function(x) { return x; }">>}},
    {ok, BaseMap} = nbson:decode(BaseBin),
    {ok, BaseBin} = nbson:encode(BaseMap).

js_ws() ->
    [{userdata, [{doc, "Tests javascriptWithScope data type BSON decoder API."}]}].
js_ws(_Config) ->
    BaseBin =
        <<61, 0, 0, 0, 15, 106, 115, 119, 115, 0, 50, 0, 0, 0, 30, 0, 0, 0, 102, 117, 110, 99, 116,
            105, 111, 110, 32, 40, 120, 41, 123, 32, 114, 101, 116, 117, 114, 110, 32, 120, 32, 42,
            32, 120, 59, 32, 125, 0, 12, 0, 0, 0, 16, 120, 0, 1, 0, 0, 0, 0, 0>>,
    BaseMap = #{<<"jsws">> => {javascript, #{<<"x">> => 1}, <<"function (x){ return x * x; }">>}},
    {ok, BaseMap} = nbson:decode(BaseBin),
    {ok, BaseBin} = nbson:encode(BaseMap).

max_key() ->
    [{userdata, [{doc, "Tests maxkey data type BSON decoder API."}]}].
max_key(_Config) ->
    BaseBin = <<14, 0, 0, 0, 127, 109, 97, 120, 95, 107, 101, 121, 0, 0>>,
    BaseMap = #{<<"max_key">> => max_key},
    {ok, BaseMap} = nbson:decode(BaseBin),
    {ok, BaseBin} = nbson:encode(BaseMap).

min_key() ->
    [{userdata, [{doc, "Tests maxkey data type BSON decoder API."}]}].
min_key(_Config) ->
    BaseBin = <<14, 0, 0, 0, 255, 109, 105, 110, 95, 107, 101, 121, 0, 0>>,
    BaseMap = #{<<"min_key">> => min_key},
    {ok, BaseMap} = nbson:decode(BaseBin),
    {ok, BaseBin} = nbson:encode(BaseMap).

null() ->
    [{userdata, [{doc, "Tests null data type BSON decoder API."}]}].
null(_Config) ->
    BaseBin = <<10, 0, 0, 0, 10, 111, 112, 116, 0, 0>>,
    BaseMap = #{<<"opt">> => null},
    {ok, BaseMap} = nbson:decode(BaseBin),
    {ok, BaseBin} = nbson:encode(BaseMap).

obj_id() ->
    [{userdata, [{doc, "Tests obj_id data type BSON decoder API."}]}].
obj_id(_Config) ->
    BaseBin = <<22, 0, 0, 0, 7, 95, 105, 100, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0>>,
    BaseMap = #{<<"_id">> => {object_id, <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1>>}},
    {ok, BaseMap} = nbson:decode(BaseBin),
    {ok, BaseBin} = nbson:encode(BaseMap).

regex() ->
    [{userdata, [{doc, "Tests regex data type BSON decoder API."}]}].
regex(_Config) ->
    BaseBin =
        <<29, 0, 0, 0, 11, 110, 97, 109, 101, 0, 47, 94, 110, 111, 109, 97, 45, 91, 48, 45, 57, 93,
            46, 42, 47, 0, 105, 0, 0>>,
    BaseMap = #{<<"name">> => {regex, <<"/^noma-[0-9].*/">>, <<"i">>}},
    {ok, BaseMap} = nbson:decode(BaseBin),
    {ok, BaseBin} = nbson:encode(BaseMap).

string() ->
    [{userdata, [{doc, "Tests string data type BSON decoder API."}]}].
string(_Config) ->
    BaseBin = <<18, 0, 0, 0, 2, 115, 116, 114, 0, 4, 0, 0, 0, 115, 116, 114, 0, 0>>,
    BaseMap = #{<<"str">> => <<"str">>},
    {ok, BaseMap} = nbson:decode(BaseBin),
    {ok, BaseBin} = nbson:encode(BaseMap).

symbols() ->
    [{userdata, [{doc, "Tests symbol data type BSON decoder API."}]}].
symbols(_Config) ->
    BaseBin = <<15, 0, 0, 0, 14, 97, 0, 3, 0, 0, 0, 97, 98, 0, 0>>,
    BaseMap = #{<<"a">> => ab},
    {ok, BaseMap} = nbson:decode(BaseBin),
    {ok, BaseBin} = nbson:encode(BaseMap).

timestamp() ->
    [{userdata, [{doc, "Tests timestamp data type BSON decoder API."}]}].
timestamp(_Config) ->
    BaseBin =
        <<24, 0, 0, 0, 17, 116, 105, 109, 101, 115, 116, 97, 109, 112, 0, 193, 80, 205, 98, 1, 0, 0,
            0, 0>>,
    BaseMap = #{<<"timestamp">> => {timestamp, 1657622721, 1}},
    {ok, BaseMap} = nbson:decode(BaseBin),
    {ok, BaseBin} = nbson:encode(BaseMap).

undef() ->
    [{userdata, [{doc, "Tests undef data type BSON decoder API."}]}].
undef(_Config) ->
    BaseBin = <<14, 0, 0, 0, 14, 98, 0, 2, 0, 0, 0, 99, 0, 0>>,
    BaseMap1 = #{<<"a">> => undefined, <<"b">> => c},
    BaseMap2 = #{<<"b">> => c},
    {ok, BaseMap2} = nbson:decode(BaseBin),
    {ok, BaseBin} = nbson:encode(BaseMap1).

proplists() ->
    [{userdata, [{doc, "Tests various previously untested cases."}]}].
proplists(_Config) ->
    {ok, <<5, 0, 0, 0, 0>>} = nbson:encode(#{}),

    {ok,
        <<46, 0, 0, 0, 4, 97, 114, 114, 0, 36, 0, 0, 0, 16, 48, 0, 1, 0, 0, 0, 2, 49, 0, 4, 0, 0, 0,
            116, 119, 111, 0, 2, 50, 0, 6, 0, 0, 0, 116, 104, 114, 101, 101, 0, 0,
            0>>} =
        nbson:encode([{<<"arr">>, [1, <<"two">>, <<"three">>]}]),

    {ok,
        <<64, 0, 0, 0, 4, 97, 114, 114, 0, 54, 0, 0, 0, 16, 48, 0, 1, 0, 0, 0, 2, 49, 0, 4, 0, 0, 0,
            116, 119, 111, 0, 2, 50, 0, 6, 0, 0, 0, 116, 104, 114, 101, 101, 0, 3, 51, 0, 15, 0, 0,
            0, 16, 102, 111, 117, 114, 0, 4, 0, 0, 0, 0, 0,
            0>>} =
        nbson:encode([{<<"arr">>, [1, <<"two">>, <<"three">>, [{<<"four">>, 4}]]}]).

various() ->
    [{userdata, [{doc, "Tests various previously untested cases."}]}].
various(_Config) ->
    {ok, <<>>} = nbson:encode(undefined),
    {ok, <<5, 0, 0, 0, 0>>} = nbson:encode(#{}),

    <<"three">> = nbson:get([<<"arr">>, <<"two">>], #{<<"arr">> => #{<<"two">> => <<"three">>}}),
    #{<<"two">> := <<"three">>} = nbson:get(<<"arr">>, #{<<"arr">> => #{<<"two">> => <<"three">>}}),
    undefined = nbson:get([<<"arr">>, <<"three">>], #{<<"arr">> => #{<<"two">> => <<"three">>}}),
    <<"three">> = nbson:get([<<"arr">>, <<"two">>], [{<<"arr">>, [{<<"two">>, <<"three">>}]}]),
    undefined = nbson:get([<<"arr">>, <<"three">>], [{<<"arr">>, [{<<"two">>, <<"three">>}]}]).
