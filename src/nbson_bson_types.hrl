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
-ifndef(nbson_bson_types).
-define(nbson_bson_types, true).

%%% MACROS
-define(DOUBLE_TYPE, 1).
-define(STRING_TYPE, 2).
-define(EMBDOC_TYPE, 3).
-define(ARRAY_TYPE, 4).
-define(BIN_TYPE, 5).
-define(UNDEF_TYPE, 6).
-define(OBJID_TYPE, 7).
-define(BOOLEAN_TYPE, 8).
-define(DATETIME_TYPE, 9).
-define(NULL_TYPE, 10).
-define(REGEX_TYPE, 11).
-define(DBPOINTER_TYPE, 12).
-define(JSCODE_TYPE, 13).
-define(SYMBOL_TYPE, 14).
-define(JSCODEWS_TYPE, 15).
-define(INT32_TYPE, 16).
-define(TIMESTAMP_TYPE, 17).
-define(INT64_TYPE, 18).
-define(MAXKEY_TYPE, 127).
-define(MINKEY_TYPE, 255).

-define(INT8(B), (B):8 / little).
-define(INT32(I), (I):32 / little - signed).
-define(INT64(I), (I):64 / little - signed).
-define(BITS96(B), (B):96 / bits).
-define(DOUBLE(D), (D):64 / little - float).
-define(CSTRING(S), (S) / binary, ?NULL).
-define(NULL, 0).

% -ifndef(nbson_bson_types)
-endif.
