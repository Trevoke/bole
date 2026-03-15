# Changelog

All notable changes to this project will be documented in this file.

## [0.2.0] - 2026-03-15

### Added
- Schema-aware tables with `create_table`, `put_row`, `get_row`, `update_row`, `delete_row`, `range_rows`
- Cell-level merge: branches modifying different columns of the same row merge cleanly
- Auto-generated UUIDv7 primary keys on every table
- Uuid module with opaque type, hex/string conversion
- New types: Bool, Float (IEEE 754, NaN rejected), Timestamp (microseconds), Blob (arbitrary bytes)
- Byte-stuffing encoding for String and Blob (handles embedded null bytes)
- Order-preserving encoding for all types
- CLI `create-table` command
- CLI `update` command

### Changed
- Schema required for all tables
- Db API uses `Tuple.t` for typed keys/values
- Db API uses `Uuid.t` for row identification
- String encoding switched to byte-stuffing (breaking on-disk format change)
- CLI `put` takes `col=val` pairs, prints generated UUID
- CLI `get`/`delete` take UUID argument

## [0.1.0] - 2026-03-14

### Added
- Prolly tree library: Hash (BLAKE2s-256), Store, Chunk, Chunker, Tree
- Diff: O(d log n) parallel descent
- Merge: three-way diff stream reconciliation
- Database layer: Db with named tables, commits, branches
- File-backed persistence in `.bole/` directory
- CLI with 10 subcommands: init, put, get, delete, commit, log, branch, switch, diff, merge
- Cross-platform binaries (Linux, macOS, Windows)
