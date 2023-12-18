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
| `I64 :: integer() when -16#80000000 =< I64, I64 =< 16#7fffffff` | 64-bit integer (18) |
| `max_key` | Max key (127) |
| `min_key` | Min key (-1) |

On deserialization, we prevent the dynamic generation of atoms by converting BSON Symbol, Max Key and Min Key values to Erlang binaries.

## Benchmarking
The BSON decoder implementation in `nbson_decoder.erl` uses [CPS](https://en.wikipedia.org/wiki/Continuation-passing_style). In this particular case, CPS leads to the use of the [sub binary delayed optimization](https://www.erlang.org/doc/efficiency_guide/binaryhandling.html#match-context) and improved efficiency in the deserialization process.

The `nbson_BENCH` script under the `bench` directory on this repository measures the decoding and encoding times for a series of BSONs containing from 1 to 1M documents using `nbson`. This escript also executes such deserializations using [bson-erlang](https://github.com/comtihon/bson-erlang), a well-known BSON encoder/decoder, for comparison purposes. To execute the benchmark yourself, please run `rebar3 as bench compile` before executing the script.

Executing the measurement using the .bson files under `test/benchmarks/data` produced the table below. Each row corresponds to measuring the decoding time of the BSONs in a given file. The first column specifies the number of documents inside each BSON file, the second column specifies the byte sizes for each of those BSONs and the third and fourth columns show the measured times in µs for nbson and bson-erlang respectively.

```
1> nbson_bench:bench().
--------------------------------------------------------------------------------------
Decoder:
--------------------------------------------------------------------------------------
    Size (documents)     File size (bytes)       Nbson Time (us)  BsonErlang Time (us)
                   1                   150                     2                     1
                  10                  2156                     0                     0
                 100                 21439                     1                     3
                1000                208773                    22                    35
               10000               2035919                   346                   847
              100000              20365952                  6155                 10322
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
Encoder:
--------------------------------------------------------------------------------------
    Size (documents)     File size (bytes)       Nbson Time (us)  BsonErlang Time (us)
                   1                   150                     0                    10
                  10                  2156                     0                     0
                 100                 21439                     1                     4
                1000                208773                    19                    29
               10000               2035919                   354                   720
              100000              20365952                  4063                  6802
--------------------------------------------------------------------------------------
```

Those used .bson files were generated using the [nbson_corpus](https://github.com/nomasystems/nbson_corpus) Erlang library.

# Examples
Check out the `nbson_SUITE.erl` file under `tests` to see some examples of BSONs and their Erlang representation.

## Support
Any doubt or suggestion? Please check out [our issue tracker](https://github.com/nomasystems/nbson/issues).
