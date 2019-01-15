#!/bin/bash

cd "$(dirname $0)"
ocamlformat -i **/*.ml **/*.mli
ocp-indent -c JaneStreet -i **/*.ml **/*.mli
dune build bin/git_print_object.exe @runtest
