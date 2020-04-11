#!/bin/bash

set -euo pipefail
shopt -s globstar

cd "$(dirname "$0")"
dune build @fmt --auto-promote || true
ocp-indent -c JaneStreet -i ./**/*.ml ./**/*.mli || true
dune build bin/git_util.exe fuse/git_fuse.exe @runtest @install
