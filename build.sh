#!/bin/sh

set -eu

cd "$(dirname "$0")"
dune build @fmt --auto-promote || true
find . "(" -name "*.c" -o -name "*.h" ")" -exec \
    clang-format -i {} + || true
dune build bin/git_util.exe fuse/git_fuse.exe @runtest @install

STATUS=$(git status --porcelain)
if [ -n "$STATUS" ]; then
  YEAR=$(date +%Y)
else
  TIMESTAMP=$(git show --no-patch --format=%ct)
  YEAR=$(date -d "@$TIMESTAMP" +%Y 2>/dev/null || date -r "$TIMESTAMP" +%Y)
fi
find . "(" -name "*.ml" -o -name "*.mli" -o -name "*.c" ")" | while read -r FILE_PATH; do
  FILE_NAME=$(basename "$FILE_PATH")
  if [ "$FILE_NAME" = "import.ml" ]; then
    continue
  fi

  LINE_COUNT=$(wc -l "$FILE_PATH" | sed "s/^ *//" | cut -f1 -d" ")
  if [ "$LINE_COUNT" -le 1 ]; then
    continue
  fi

  if ! grep -q "Copyright (C) \([0-9]\{4\}-\)\?${YEAR}  Bogdan-Cristian Tataroiu" "$FILE_PATH"; then
    echo "File missing GPL License header: $FILE_PATH"
    exit 1
  fi
done
