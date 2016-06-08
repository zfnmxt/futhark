#!/bin/sh
#
# Measure Futhark's radix sort implementation on large datasets and with
# different degrees of write optimisations enabled.

cd "$(dirname "$0")/.."
for x in ../futhark-benchmarks/misc/radix_sort/*_benchmark.fut; do
    echo "# $x"
    for version in clean fusion elimination fusion-elimination; do
        echo "## $version"
        futhark-bench --compiler=bin-$version/futhark-opencl $x
    done
    echo
    echo
done
