# Typed Tuples Design

## What we're building

A `Tuple` module for order-preserving encoding of typed values (Int64, String), enabling composite primary keys that sort correctly under byte comparison. This is step A of the A→B→C path toward schema-aware tables with cell-level merge.

## The path: A → B → C

- **A (this step): Tuple encoding module.** Standalone encode/decode for typed value lists. Order-preserving. Tree still stores bytes; callers encode/decode at the Db boundary.
- **B: Typed keys and values at Db layer.** Convenience functions that encode/decode tuples internally. Tree unchanged.
- **C: Schema-aware tables.** Column names and types. `create_table`, `put_row`, `get_row`. Schema hash stored in `Db_state`. Cell-level merge — different columns on different branches merge cleanly.

Each layer only adds; nothing changes underneath.

## Design decisions

- **Int64 and String only.** Float and Blob deferred until tests demand them.
- **Self-describing encoding.** Type tags prefix each value, so decode doesn't need external schema.
- **Tree stays byte-oriented.** Typing is above the tree, not inside it. Same approach as Dolt.
- **Db.range added.** Thin wrapper over Tree.range for table-level range scans.

## Interface

```ocaml
(* tuple.mli *)
type value =
  | Int64 of int64
  | String of string

type t = value list

val encode : t -> string
val decode : string -> t
```

## Encoding format

Each value is prefixed by a 1-byte type tag. Values concatenated within a tuple.

**Int64 (tag 0x01):** 8 bytes big-endian with sign-bit flip. XOR the most significant byte with 0x80. This maps `Int64.min_int` to `0x00..00` and `Int64.max_int` to `0xFF..FF`, giving correct unsigned byte ordering for signed integers.

**String (tag 0x02):** 2-byte BE length, then string bytes, then `0x00` terminator. The terminator ensures `"ab"` < `"abc"` (shorter sorts before longer with same prefix).

## Db.range

```ocaml
val range : t -> table:string -> (string * string) Seq.t
```

Looks up table root, delegates to `Tree.range`. Returns raw encoded bytes.

## Tests

**Tuple unit tests:**
- Int64 round-trip
- String round-trip
- Multi-field round-trip
- Int64 byte ordering: -1, 0, 1, 100 sort correctly via String.compare
- String byte ordering: "ab" < "abc" < "b"
- Composite ordering: ("alice", 10) < ("alice", 20) < ("bob", 1)
- Empty tuple round-trip

**Acceptance tests:**
- Int64 keys sort numerically in Db.range
- Composite keys sort correctly in Db.range

## Future acceptance tests (steps B and C, commented out)

- Schema-aware table: create_table, put_row, get_row with named columns
- Cell-level merge: different columns on different branches merge cleanly
