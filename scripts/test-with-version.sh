#!/usr/bin/env bash

set -e

export OCAML_VERSION="${1}"

make clean-for-ci
opam switch "${OCAML_VERSION}"
eval `opam config env`
opam update
# Our constraints are wrong I believe. We need this version.
make install
make test-ci
git diff --exit-code
