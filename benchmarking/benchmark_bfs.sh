#!/bin/sh
#
# Measure Futhark's BFS implementation on large datasets and with different
# degrees of write optimisations enabled.

cd "$(dirname "$0")/.."
for x in ../futhark-benchmarks/rodinia/bfs/*_more.fut; do
    echo "# $x"
    for version in clean fusion elimination fusion-elimination; do
        echo "## $version"
        futhark-bench --compiler=bin-$version/futhark-opencl $x
    done
    echo
    echo
done
