#!/bin/sh
#
# Build the futhark compiler with varying support for write optimisations.

set -e # Exit on first error.

define() {
    echo --ghc-options -D$1=1
}

install_with() {
    name=$1
    shift
    stack clean
    stack build --ghc-options -cpp $@
    stack install
    bin=bin-$name
    mkdir $bin
    cp ~/.local/bin/futhark* $bin
}

flags_fusion="$(define MAP_WRITE_FUSION) $(define WRITE_WRITE_FUSION)"
flags_elimination="$(define WRITE_REPLICATE_ELIMINATION) $(define WRITE_IOTA_ELIMINATION)"

install_with clean
install_with fusion $flags_fusion
install_with elimination $flags_elimination
install_with fusion-elimination $flags_fusion $flags_elimination
