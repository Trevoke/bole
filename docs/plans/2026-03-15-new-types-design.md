# New Types Design: Bool, Float, Timestamp, Blob

## What we're building

Four new types for Tuple.value and Schema.column_type: Bool, Float, Timestamp, and Blob. Also fix String encoding to use byte-stuffing (handles embedded null bytes correctly).

## Design decisions

- **Byte-stuffing for String and Blob.** Both use `\x00\xFF` escaping with `\x00\x00` terminator. Handles arbitrary byte content, preserves lexicographic ordering. Breaking change to existing String encoding — pre-1.0, acceptable.
- **Reject NaN for Float.** NaN in a database column is always a bug. `Tuple.encode` raises on NaN.
- **Timestamp as microseconds.** Int64 encoding internally (sign-bit flip, big-endian). Distinct type for semantic clarity and CLI display.
- **All encodings are order-preserving.** Even though all keys are UUIDs now, correct ordering supports future secondary indexes.

## Encoding table

| Type | Tag | Encoding | Order |
|------|-----|----------|-------|
| Int64 | 0x01 | 8B big-endian, sign-bit flip | Numeric |
| String | 0x02 | byte-stuffed + `\x00\x00` terminator | Lexicographic |
| Uuid | 0x03 | 16B raw | Chronological |
| Bool | 0x04 | 1B (0x00 false, 0x01 true) | false < true |
| Float | 0x05 | 8B IEEE 754, sign manipulation, NaN rejected | Numeric |
| Timestamp | 0x06 | 8B big-endian, sign-bit flip (microseconds) | Chronological |
| Blob | 0x07 | byte-stuffed + `\x00\x00` terminator | Lexicographic |

## Byte-stuffing encoding

Encode: replace each `\x00` in data with `\x00\xFF`, then append `\x00\x00` as terminator.

Decode: scan for `\x00\x00` (unescaped), collecting bytes. When `\x00\xFF` is encountered, emit `\x00`. When `\x00\x00` is encountered, stop.

Ordering: preserved because `\x00\xFF` (escaped null) sorts after `\x00\x00` (terminator), correctly placing shorter values before longer ones with the same prefix.

## Float sign manipulation

IEEE 754 doubles are 8 bytes. For order-preserving encoding:
- If sign bit is set (negative): flip ALL 64 bits
- If sign bit is clear (positive or +0): flip only the sign bit

This maps the full float range to unsigned byte order: -inf → 0x00..., -0 → 0x7F..., +0 → 0x80..., +inf → 0xFF...

On decode: if high bit is set (was positive), flip only sign bit. If high bit is clear (was negative), flip all bits.

NaN: `Float.is_nan` check before encoding, raise `Invalid_argument`.

## Timestamp

Unix timestamp in microseconds since epoch. Stored as Int64 with sign-bit flip (same encoding as Int64, different tag). Microsecond precision covers ~±290,000 years.

## Changes needed

- `Tuple.value`: add `Bool of bool | Float of float | Timestamp of int64 | Blob of string`
- `Tuple.encode/decode`: add cases for tags 0x04-0x07, fix String to use byte-stuffing
- `Schema.column_type`: add `Bool | Float | Timestamp | Blob`
- `Schema.encode/decode`: add type bytes 0x04-0x07
- CLI: update `render_tuple`, `parse_assignments`, `render_row` for new types
- Tests: round-trip and ordering tests for each new type

## Tests

Per type:
- Round-trip encode/decode
- Ordering (for types where ordering matters)

Additional:
- String with embedded null bytes round-trips correctly
- Blob with embedded null bytes round-trips correctly
- Float rejects NaN
- Float ordering: -inf < -1.0 < -0.0 < +0.0 < 1.0 < +inf
- Timestamp ordering: earlier < later
