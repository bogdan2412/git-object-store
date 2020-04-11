#!/bin/bash

shopt -s globstar

cd "$(dirname $0)"
dune build @fmt --auto-promote
ocp-indent -c JaneStreet -i **/*.ml **/*.mli
dune build bin/git_util.exe fuse/git_fuse.exe @runtest @install
