#!/bin/sh

set -eu

cd "$(dirname "$0")"
dune build @fmt --auto-promote || true
find . "(" -name "*.ml" -o -name "*.mli" ")" -exec \
    ocp-indent -c JaneStreet -i {} + || true
dune build bin/git_util.exe fuse/git_fuse.exe @runtest @install
