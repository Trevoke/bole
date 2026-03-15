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

echo "=== create-table ==="
$BOLE create-table users key:string value:string --pk key

echo "=== put/get ==="
$BOLE put users key=alice value=admin
$BOLE put users key=bob value=editor
test "$($BOLE get users alice)" = "key=alice value=admin"
test "$($BOLE get users bob)" = "key=bob value=editor"

echo "=== commit ==="
$BOLE put users key=alice value=superadmin
C1=$($BOLE commit -m "initial")
echo "commit1: $C1"

$BOLE put users key=alice value=megadmin
C2=$($BOLE commit -m "promote alice")
echo "commit2: $C2"

echo "=== log ==="
$BOLE log

echo "=== diff ==="
$BOLE diff "$C1" "$C2" users

echo "=== branch and merge ==="
$BOLE branch feature
$BOLE put users key=carol value=new
$BOLE commit -m "add carol on feature" > /dev/null
$BOLE switch main
# Verify alice is still megadmin on main
test "$($BOLE get users alice)" = "key=alice value=megadmin"
$BOLE merge feature
test "$($BOLE get users carol)" = "key=carol value=new"
test "$($BOLE get users alice)" = "key=alice value=megadmin"

echo ""
echo "ALL CLI TESTS PASSED"
