#!/bin/bash

shopt -s globstar

cd "$(dirname $0)"
ocamlformat -i **/*.ml **/*.mli
ocp-indent -c JaneStreet -i **/*.ml **/*.mli
dune build bin/git_util.exe @runtest
