#!/bin/bash
set -euo pipefail

# Build first and resolve binary path
dune build bin/main.exe
BOLE="$(pwd)/_build/default/bin/main.exe"

DIR=$(mktemp -d)
cleanup() {
  rm -rf "$DIR"
}
trap cleanup EXIT

cd "$DIR"

echo "=== init ==="
$BOLE init

echo "=== put/get ==="
$BOLE put users alice admin
$BOLE put users bob editor
test "$($BOLE get users alice)" = "admin"
test "$($BOLE get users bob)" = "editor"
# Verify get still works after commit
$BOLE commit -m "save working state" > /dev/null
test "$($BOLE get users alice)" = "admin"
test "$($BOLE get users bob)" = "editor"

echo "=== commit ==="
$BOLE put users alice superadmin
C1=$($BOLE commit -m "initial")
echo "commit1: $C1"

$BOLE put users alice megadmin
C2=$($BOLE commit -m "promote alice")
echo "commit2: $C2"

echo "=== log ==="
$BOLE log

echo "=== diff ==="
$BOLE diff "$C1" "$C2" users

echo "=== branch and merge ==="
$BOLE branch feature
$BOLE put users carol new
$BOLE commit -m "add carol on feature" > /dev/null
$BOLE switch main
test "$($BOLE get users alice)" = "megadmin"
$BOLE merge feature
test "$($BOLE get users carol)" = "new"
test "$($BOLE get users alice)" = "megadmin"

echo ""
echo "ALL CLI TESTS PASSED"
