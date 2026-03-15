# Auto-Generated UUID Primary Keys Design

## What we're building

Every table gets an automatic UUIDv7 primary key (`_id` column). Users never specify or manage primary keys. The Db API uses `Uuid.t` for row identification. New `Uuid` module for UUIDv7 generation with opaque type. `put_row` becomes an insert (generates UUID), separate `update_row` for modifications.

## Design decisions

- **Always auto-UUID.** No user-specified primary keys. The `_id` column is prepended automatically by `Schema.create`. Primary keys don't hold business domain knowledge.
- **UUIDv7 (time-ordered).** Timestamp prefix means sequential inserts cluster in the same leaf chunks — good for prolly tree write performance. Sorts chronologically under byte comparison.
- **Uuid as a proper type.** Opaque `Uuid.t` with `to_hex`/`of_hex`/`of_string`. Not a plain string. `Tuple.Uuid of Uuid.t` variant. Type-safe API.
- **Insert vs update separation.** `put_row` always inserts (generates new UUID, returns it). `update_row` modifies an existing row by UUID. Clear semantics, no accidental overwrites.
- **Schema still has primary_key field.** Always `["_id"]`, filled in automatically. The schema is fully self-describing when serialized.
- **Duplicate rows possible across branches.** Two branches inserting "the same" data get different UUIDs. This is the tradeoff of auto-UUIDs — no natural key deduplication. Uniqueness constraints on non-key columns (future work) would address this.

## Uuid module

```ocaml
(* uuid.mli *)
type t

val v7 : unit -> t
(* Generate a new UUIDv7: 48-bit Unix timestamp (ms) +
   4-bit version (0111) + 12-bit random +
   2-bit variant (10) + 62-bit random *)

val to_raw_string : t -> string   (* 16 bytes for storage/encoding *)
val of_raw_string : string -> t   (* from 16 raw bytes *)
val to_hex : t -> string          (* 32 hex chars, no dashes *)
val of_hex : string -> t          (* strict: 32 hex chars *)
val of_string : string -> t       (* permissive: accepts dashes *)
val equal : t -> t -> bool
val compare : t -> t -> int
```

Implementation uses `Unix.gettimeofday()` for timestamp, `Random.bits()` for randomness. Adds `unix` to library dune dependencies.

## Tuple changes

```ocaml
type value =
  | Int64 of int64
  | String of string
  | Uuid of Uuid.t
```

Encoding: tag `0x03` + 16 raw bytes. No length prefix, no terminator. UUIDv7 bytes sort chronologically under byte comparison.

## Schema changes

```ocaml
type column_type = Int64 | Str | Uuid

type t = {
  columns : (string * column_type) list;
  primary_key : string list;
}

val create : columns:(string * column_type) list -> t
(* Prepends ("_id", Uuid) to columns, sets primary_key = ["_id"] *)
```

Binary format: type byte `0x03` for Uuid. primary_key section still encoded (always ["_id"]).

## Db API

```ocaml
val create_table : t -> table:string -> schema:Schema.t -> t

val put_row : t -> table:string -> row:(string * Tuple.value) list -> Uuid.t * t
(* Insert: generates UUIDv7, returns (uuid, updated_db) *)

val update_row : t -> table:string -> id:Uuid.t -> row:(string * Tuple.value) list -> t
(* Update: modifies existing row by UUID *)

val get_row : t -> table:string -> id:Uuid.t -> (string * Tuple.value) list option
(* Lookup by UUID, returns full row including _id *)

val delete_row : t -> table:string -> id:Uuid.t -> t
(* Delete by UUID *)

val range_rows : t -> table:string -> (string * Tuple.value) list Seq.t
(* Each row includes ("_id", Uuid uuid) *)
```

## Tests

**Uuid unit tests:**
- v7 generates 16-byte values
- v7 generates unique values
- v7 values sort chronologically (generate two, first < second)
- to_hex / of_hex round-trip
- of_string accepts dashes
- of_hex rejects invalid input

**Updated acceptance tests:**
- All tests convert to auto-UUID: `put_row` returns UUID, `get_row`/`update_row`/`delete_row` take `Uuid.t`
- Cell-level merge test uses `update_row` to modify rows on branches

**CLI updates:**
- `create-table` no longer takes `--pk`
- `put` takes `col=val` pairs (no id), prints generated UUID
- `get` takes a UUID
- `delete` takes a UUID
- `update` new command: `bole update <table> <uuid> col=val ...`
