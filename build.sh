#!/bin/bash

set -euo pipefail

cd "$(dirname "$0")"
ocamlformat -i ./**/*.ml ./**/*.mli || true
ocp-indent -c JaneStreet -i ./**/*.ml ./**/*.mli || true
dune build bin/git_print_object.exe @runtest
