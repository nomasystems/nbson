# nbson
![nbson](https://github.com/nomasystems/nbson/actions/workflows/build.yml/badge.svg)

`nbson` is an OTP library to encode/decode BSON documents from/to Erlang terms.

## Setup

```erl
%%% e.g., rebar.config
{deps, [
    {nbson, {git, "git@github.com:nomasystems/nbson.git", {branch, "main"}}}
]}.
```

## Features
`nbson` exposes utilities via its API that allows you to:

| Function | Description |
| -------- | ----------- |
| `nbson:encode/1` | Serialize an Erlang term to BSON |
| `nbson:decode/2` | Deserialize a BSON to an Erlang term |

## Implementation
`nbson` represents BSONs as Erlang proplists with tuple values only.

Produced proplists' keys are binaries and value types depend on the associated BSON type.

The following table represents the association between Erlang types and BSON types on serialization.

| Erlang Data Type | BSON Types |
| ---------------- | ---------- |
| `float()` | Double (1) |
| `binary()` | String (2) |
| `[ tuple() \| _ ] ` | Object (3) |
| `list()` | Array (4) |
| `{data, binary, binary()}` | Binary data (5) |
| `{vector, int8, [integer()]}` | Binary Vector INT8 (5, subtype 9) |
| `{vector, float32, [float()]}` | Binary Vector FLOAT32 (5, subtype 9) |
| `{vector, packed_bit, binary()}` | Binary Vector PACKED_BIT (5, subtype 9) |
| `{vector, packed_bit, binary(), 0..7}` | Binary Vector PACKED_BIT with padding (5, subtype 9) |
| `undefined` | Undefined (6) |
| `{object_id, <<_:96>>}` | ObjectId (7) |
| `false \| true` | Boolean (8) |
| `{integer(), integer(), integer()}` | Date (9) |
| `null` | Null (10) |
| `{regex, charlist(), charlist()}` | Regular Expression (11) |
| `{pointer, binary(), <<_:96>>}` | DBPointer (12) |
| `{javascript, [{}], binary()}` | JavaScript (13) |
| `Atom :: atom() when Atom =/= min_key, Atom =/= max_key` | Symbol (14)
| `{javascript, [ tuple() \| _ ], binary()}` | JavaScript code with scope (15) |
| `I32 :: integer() when -16#80000000 =< I32, I32 =< 16#7fffffff` | 32-bit integer (16) |
| `{timestamp, binary(), binary()}` | Timestamp (17) |
| `I64 :: integer() when -16#8000000000000000 =< I64, I64 =< 16#7fffffffffffffff` | 64-bit integer (18) |
| `{long, I64 :: integer()} when -16#8000000000000000 =< I64, I64 =< 16#7fffffffffffffff` | 64-bit integer (18) |
| `max_key` | Max key (127) |
| `min_key` | Min key (-1) |

On deserialization, we prevent the dynamic generation of atoms by converting BSON Symbol, Max Key and Min Key values to Erlang binaries.

### Binary Vectors

`nbson` supports [BSON Binary Subtype 9](https://github.com/mongodb/specifications/blob/master/source/bson-binary-vector/bson-binary-vector.md) for efficient storage of dense numeric vectors, useful for AI/ML applications and semantic search.

| Vector Type | Erlang Representation | Value Range |
| ----------- | --------------------- | ----------- |
| INT8 | `{vector, int8, [integer()]}` | -128 to 127 |
| FLOAT32 | `{vector, float32, [float()]}` | IEEE 754 single-precision (supports `infinity`, `neg_infinity`) |
| PACKED_BIT | `{vector, packed_bit, binary()}` | Raw bytes (0-255) representing packed bits |
| PACKED_BIT | `{vector, packed_bit, binary(), Padding}` | With padding (0-7 bits to ignore in final byte) |

```erlang
%% INT8 vector
{ok, Bin} = nbson:encode(#{<<"embedding">> => {vector, int8, [127, -128, 0, 42]}}).
{ok, #{<<"embedding">> := {vector, int8, [127, -128, 0, 42]}}} = nbson:decode(Bin).

%% FLOAT32 vector with special values
{ok, Bin} = nbson:encode(#{<<"v">> => {vector, float32, [1.5, infinity, neg_infinity]}}).

%% PACKED_BIT vector with padding (13 bits = 2 bytes with 3 bits padding)
{ok, Bin} = nbson:encode(#{<<"bits">> => {vector, packed_bit, <<255, 248>>, 3}}).
```

## Benchmarking
The BSON decoder implementation in `nbson_decoder.erl` uses [CPS](https://en.wikipedia.org/wiki/Continuation-passing_style). In this particular case, CPS leads to the use of the [sub binary delayed optimization](https://www.erlang.org/doc/efficiency_guide/binaryhandling.html#match-context) and improved efficiency in the deserialization process.

The [nbson_bench repository](https://github.com/nomasystems/nbson_bench) hosts benchmarks to measure the decoding and encoding times of different BSONs compared to other equivalent tools.

# Examples
Check out the `nbson_SUITE.erl` file under `tests` to see some examples of BSONs and their Erlang representation.

## Support
Any doubt or suggestion? Please check out [our issue tracker](https://github.com/nomasystems/nbson/issues).
