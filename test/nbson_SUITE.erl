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
        bin,
        boolean,
        datetime,
        db_pointer,
        double,
        embdoc,
        int64,
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
        undef
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
    {BaseMap, <<>>} = nbson:decode(BaseBin),
    BaseBin = nbson:encode(BaseMap).

bin() ->
    [{userdata, [{doc, "Tests bin data type BSON decoder API."}]}].
bin(_Config) ->
    BaseBin =
        <<23, 0, 0, 0, 5, 98, 105, 110, 0, 8, 0, 0, 0, 0, 131, 107, 0, 4, 110, 111, 109, 97, 0>>,
    BaseMap = #{<<"bin">> => {data, binary, <<131, 107, 0, 4, 110, 111, 109, 97>>}},
    {BaseMap, <<>>} = nbson:decode(BaseBin),
    BaseBin = nbson:encode(BaseMap).

boolean() ->
    [{userdata, [{doc, "Tests boolean data type BSON decoder API."}]}].
boolean(_Config) ->
    BaseBin = <<20, 0, 0, 0, 8, 102, 97, 108, 115, 101, 0, 0, 8, 116, 114, 117, 101, 0, 1, 0>>,
    BaseMap = #{<<"true">> => true, <<"false">> => false},
    {BaseMap, <<>>} = nbson:decode(BaseBin),
    BaseBin = nbson:encode(BaseMap).

datetime() ->
    [{userdata, [{doc, "Tests datetime data type BSON decoder API."}]}].
datetime(_Config) ->
    BaseBin = <<17, 0, 0, 0, 9, 100, 116, 0, 223, 131, 98, 249, 127, 1, 0, 0, 0>>,
    BaseMap = #{<<"dt">> => {1649, 156457, 439000}},
    {BaseMap, <<>>} = nbson:decode(BaseBin),
    BaseBin = nbson:encode(BaseMap).

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
    {BaseMap, <<>>} = nbson:decode(BaseBin),
    BaseBin = nbson:encode(BaseMap).

double() ->
    [{userdata, [{doc, "Tests double data type BSON decoder API."}]}].
double(_Config) ->
    BaseBin = <<18, 0, 0, 0, 1, 111, 110, 101, 0, 0, 0, 0, 0, 0, 0, 240, 63, 0>>,
    BaseMap = #{<<"one">> => 1.0},
    {BaseMap, <<>>} = nbson:decode(BaseBin),
    BaseBin = nbson:encode(BaseMap).

embdoc() ->
    [{userdata, [{doc, "Tests embdoc data type BSON decoder API."}]}].
embdoc(_Config) ->
    BaseBin =
        <<28, 0, 0, 0, 3, 100, 111, 99, 0, 18, 0, 0, 0, 2, 111, 110, 101, 0, 4, 0, 0, 0, 111, 110,
            101, 0, 0, 0>>,
    BaseMap = #{<<"doc">> => #{<<"one">> => <<"one">>}},
    {BaseMap, <<>>} = nbson:decode(BaseBin),
    BaseBin = nbson:encode(BaseMap).

int64() ->
    [{userdata, [{doc, "Tests int64 data type BSON decoder API."}]}].
int64(_Config) ->
    BaseBin = <<20, 0, 0, 0, 18, 105, 110, 116, 54, 52, 0, 188, 104, 151, 147, 227, 13, 1, 23, 0>>,
    BaseMap = #{<<"int64">> => 1657621408933963964},
    {BaseMap, <<>>} = nbson:decode(BaseBin),
    BaseBin = nbson:encode(BaseMap).

js() ->
    [{userdata, [{doc, "Tests javascript data type BSON decoder API."}]}].
js(_Config) ->
    BaseBin =
        <<41, 0, 0, 0, 13, 99, 111, 100, 101, 0, 26, 0, 0, 0, 102, 117, 110, 99, 116, 105, 111, 110,
            40, 120, 41, 32, 123, 32, 114, 101, 116, 117, 114, 110, 32, 120, 59, 32, 125, 0, 0>>,
    BaseMap = #{<<"code">> => {javascript, #{}, <<"function(x) { return x; }">>}},
    {BaseMap, <<>>} = nbson:decode(BaseBin),
    BaseBin = nbson:encode(BaseMap).

js_ws() ->
    [{userdata, [{doc, "Tests javascriptWithScope data type BSON decoder API."}]}].
js_ws(_Config) ->
    BaseBin =
        <<61, 0, 0, 0, 15, 106, 115, 119, 115, 0, 50, 0, 0, 0, 30, 0, 0, 0, 102, 117, 110, 99, 116,
            105, 111, 110, 32, 40, 120, 41, 123, 32, 114, 101, 116, 117, 114, 110, 32, 120, 32, 42,
            32, 120, 59, 32, 125, 0, 12, 0, 0, 0, 16, 120, 0, 1, 0, 0, 0, 0, 0>>,
    BaseMap = #{<<"jsws">> => {javascript, #{<<"x">> => 1}, <<"function (x){ return x * x; }">>}},
    {BaseMap, <<>>} = nbson:decode(BaseBin),
    BaseBin = nbson:encode(BaseMap).

max_key() ->
    [{userdata, [{doc, "Tests maxkey data type BSON decoder API."}]}].
max_key(_Config) ->
    BaseBin = <<14, 0, 0, 0, 127, 109, 97, 120, 95, 107, 101, 121, 0, 0>>,
    BaseMap = #{<<"max_key">> => max_key},
    {BaseMap, <<>>} = nbson:decode(BaseBin),
    BaseBin = nbson:encode(BaseMap).

min_key() ->
    [{userdata, [{doc, "Tests maxkey data type BSON decoder API."}]}].
min_key(_Config) ->
    BaseBin = <<14, 0, 0, 0, 255, 109, 105, 110, 95, 107, 101, 121, 0, 0>>,
    BaseMap = #{<<"min_key">> => min_key},
    {BaseMap, <<>>} = nbson:decode(BaseBin),
    BaseBin = nbson:encode(BaseMap).

null() ->
    [{userdata, [{doc, "Tests null data type BSON decoder API."}]}].
null(_Config) ->
    BaseBin = <<10, 0, 0, 0, 10, 111, 112, 116, 0, 0>>,
    BaseMap = #{<<"opt">> => null},
    {BaseMap, <<>>} = nbson:decode(BaseBin),
    BaseBin = nbson:encode(BaseMap).

obj_id() ->
    [{userdata, [{doc, "Tests obj_id data type BSON decoder API."}]}].
obj_id(_Config) ->
    BaseBin = <<22, 0, 0, 0, 7, 95, 105, 100, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0>>,
    BaseMap = #{<<"_id">> => {object_id, <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1>>}},
    {BaseMap, <<>>} = nbson:decode(BaseBin),
    BaseBin = nbson:encode(BaseMap).

regex() ->
    [{userdata, [{doc, "Tests regex data type BSON decoder API."}]}].
regex(_Config) ->
    BaseBin =
        <<29, 0, 0, 0, 11, 110, 97, 109, 101, 0, 47, 94, 110, 111, 109, 97, 45, 91, 48, 45, 57, 93,
            46, 42, 47, 0, 105, 0, 0>>,
    BaseMap = #{<<"name">> => {regex, <<"/^noma-[0-9].*/">>, <<"i">>}},
    {BaseMap, <<>>} = nbson:decode(BaseBin),
    BaseBin = nbson:encode(BaseMap).

string() ->
    [{userdata, [{doc, "Tests string data type BSON decoder API."}]}].
string(_Config) ->
    BaseBin = <<18, 0, 0, 0, 2, 115, 116, 114, 0, 4, 0, 0, 0, 115, 116, 114, 0, 0>>,
    BaseMap = #{<<"str">> => <<"str">>},
    {BaseMap, <<>>} = nbson:decode(BaseBin),
    BaseBin = nbson:encode(BaseMap).

symbols() ->
    [{userdata, [{doc, "Tests symbol data type BSON decoder API."}]}].
symbols(_Config) ->
    BaseBin = <<15, 0, 0, 0, 14, 97, 0, 3, 0, 0, 0, 97, 98, 0, 0>>,
    BaseMap = #{<<"a">> => ab},
    {BaseMap, <<>>} = nbson:decode(BaseBin),
    BaseBin = nbson:encode(BaseMap).

timestamp() ->
    [{userdata, [{doc, "Tests timestamp data type BSON decoder API."}]}].
timestamp(_Config) ->
    BaseBin =
        <<24, 0, 0, 0, 17, 116, 105, 109, 101, 115, 116, 97, 109, 112, 0, 193, 80, 205, 98, 1, 0, 0,
            0, 0>>,
    BaseMap = #{<<"timestamp">> => {timestamp, 1657622721, 1}},
    {BaseMap, <<>>} = nbson:decode(BaseBin),
    BaseBin = nbson:encode(BaseMap).

undef() ->
    [{userdata, [{doc, "Tests undef data type BSON decoder API."}]}].
undef(_Config) ->
    BaseBin = <<8, 0, 0, 0, 6, 97, 0, 0>>,
    BaseMap = #{<<"a">> => undefined},
    {BaseMap, <<>>} = nbson:decode(BaseBin),
    BaseBin = nbson:encode(BaseMap).
