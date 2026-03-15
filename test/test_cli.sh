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
$BOLE create-table users name:string email:string

echo "=== put/get ==="
ID1=$($BOLE put users name=alice email=alice@ex.com)
echo "inserted: $ID1"
$BOLE get users "$ID1" | grep -q "name=alice"
$BOLE get users "$ID1" | grep -q "email=alice@ex.com"

ID2=$($BOLE put users name=bob email=bob@ex.com)
echo "inserted: $ID2"

echo "=== commit ==="
C1=$($BOLE commit -m "initial")
echo "commit1: $C1"

echo "=== update ==="
$BOLE update users "$ID1" name=Alice email=alice@newdomain.com
C2=$($BOLE commit -m "update alice")
echo "commit2: $C2"

echo "=== get after update ==="
$BOLE get users "$ID1" | grep -q "name=Alice"
$BOLE get users "$ID1" | grep -q "email=alice@newdomain.com"

echo "=== log ==="
$BOLE log

echo "=== diff ==="
$BOLE diff "$C1" "$C2" users

echo "=== branch and merge ==="
$BOLE branch feature
ID3=$($BOLE put users name=carol email=carol@ex.com)
$BOLE commit -m "add carol" > /dev/null
$BOLE switch main
$BOLE merge feature
$BOLE get users "$ID3" | grep -q "name=carol"

echo ""
echo "ALL CLI TESTS PASSED"
