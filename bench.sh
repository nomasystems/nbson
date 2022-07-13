#!/bin/bash
rebar3 as bench compile
erl -pa _build/bench/lib/argparse/ebin -pa _build/bench/lib/bson/ebin -pa _build/bench/lib/erlperf/ebin -pa _build/bench/lib/nbson/ebin -pa _build/bench/lib/nbson/bench -eval "nbson_bench:bench(), init:stop()."
