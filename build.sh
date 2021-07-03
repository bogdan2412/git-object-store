#!/bin/bash

shopt -s globstar

cd "$(dirname $0)"
dune build @fmt --auto-promote
find . "(" -name "*.ml" -o -name "*.mli" ")" -exec \
    ocp-indent -c JaneStreet -i {} +
dune build bin/git_util.exe fuse/git_fuse.exe @runtest @install
