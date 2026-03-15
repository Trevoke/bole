# Schema-Aware Tables Design (Tuples Step C)

## What we're building

A `Schema` module for table definitions (column names, types, primary key). Schema-aware table operations (`create_table`, `put_row`, `get_row`, `delete_row`, `range_rows`) that replace the raw tuple API. Cell-level merge that resolves conflicts when two branches modify different columns of the same row. Schemas are required for all tables.

## Design decisions

- **Schema required for all tables.** This is a relational database. Every table has a schema with a primary key. Raw `put`/`find` go away from the public API.
- **Schema stored content-addressed.** Serialized, stored in Store, referenced by hash in Db_state. Same pattern as everything else.
- **Key/value split per research doc.** Primary key columns encode into the tree key tuple, non-key columns encode into the tree value tuple. No redundancy.
- **Association list rows.** `put_row` takes `(string * Tuple.value) list`, `get_row` returns the same. Column names are explicit at call sites.
- **Positional key tuple.** `get_row` and `delete_row` take `key:Tuple.t` — primary key values in schema-declared order.
- **Cell-level merge in Db.merge.** When Merge reports a whole-value conflict, Db loads the schema, decodes values, compares field-by-field against base. Non-overlapping column changes merge cleanly. Overlapping changes remain conflicts.
- **Door left open via layering.** Schema enforcement is in Db, not Tree. Tree stays a generic sorted byte map. Other store types (document, columnar) could build different modules on top of Tree.

## Schema module

```ocaml
(* schema.mli *)
type column_type = Int64 | Str

type t = {
  columns : (string * column_type) list;
  primary_key : string list;
}

val create : columns:(string * column_type) list -> primary_key:string list -> t
val encode : t -> string
val decode : string -> t
```

`create` validates that primary key columns exist in the column list.

Binary format: `[column_count: 2B BE] [columns...] [pk_count: 2B BE] [pk_names...]` where each column is `[name_len: 2B BE] [name] [type: 1B]` (0x01 = Int64, 0x02 = Str) and each pk_name is `[len: 2B BE] [name]`.

## Db_state changes

`table_entry` gains a required schema hash:

```ocaml
type table_entry = { name : string; root : Hash.t; schema : Hash.t }
```

Binary format adds schema_hash (32B) after root_hash. Breaking change — pre-schema databases won't load.

## Db internal changes

The tables map changes from `Hash.t StringMap.t` to:

```ocaml
type table_state = { root : Hash.t; schema : Hash.t }
(* tables : table_state StringMap.t *)
```

## Db public API

```ocaml
val create_table : t -> table:string -> schema:Schema.t -> t
val put_row : t -> table:string -> row:(string * Tuple.value) list -> t
val get_row : t -> table:string -> key:Tuple.t -> (string * Tuple.value) list option
val delete_row : t -> table:string -> key:Tuple.t -> t
val range_rows : t -> table:string -> (string * Tuple.value) list Seq.t
val diff : t -> from:Hash.t -> to_:Hash.t -> table:string -> diff_entry Seq.t
val merge : t -> ours:string -> theirs:string -> merge_result
```

Old `put`/`find`/`delete`/`range` removed from public API.

### Operation details

- **create_table**: encode schema, store it, record hash in table_state with empty tree root.
- **put_row**: load schema, split row into key columns and value columns by schema order, encode each as Tuple.t, call Tree.put.
- **get_row**: load schema, encode key tuple, Tree.find, decode value tuple, reconstruct full row by merging key + value column names.
- **delete_row**: load schema, encode key tuple, Tree.delete.
- **range_rows**: load schema, Tree.range, decode each pair, reconstruct rows.

## Cell-level merge

When Db.merge gets a Merge.conflict for a table with a schema:

1. Decode base, ours, theirs value tuples
2. Compare each field position against base:
   - ours[i] = base[i] → ours didn't change this field
   - theirs[i] = base[i] → theirs didn't change this field
   - Both changed, same value → agree
   - Both changed, different values → real column conflict
3. No real conflicts → construct merged value, emit as Put
4. Any real conflicts → emit as Db.conflict

Edge case: base = None (both added same key) → no field-by-field resolution, full-value conflict.

## Tests

**Schema unit tests:** round-trip, pk validation.

**All existing acceptance tests updated** to use create_table/put_row/get_row.

**New acceptance tests:**
- Test 10: Schema-aware table — create_table, put_row, get_row with named columns
- Test 11: Cell-level merge — different columns on different branches merge cleanly
