# Step 9 Design: Performance Property Tests

## What we're building

Two performance property tests that verify the core algorithmic guarantees of prolly trees: structural sharing (O(log n) new chunks per mutation) and diff efficiency (O(d log n) chunk reads). These require instrumenting the Store with call counters.

## Design decisions

- **Store instrumentation:** Add `get_count`, `put_count`, and `reset_stats` to the Store module. Simple int ref counters incremented on each call.
- **Unit tests, not QCheck:** These are complexity assertions on fixed data sizes. A deterministic 1000-key tree with known target_size gives predictable height, making bounds easy to reason about. Randomness doesn't help here.
- **Generous bounds:** Structural sharing asserts ≤ 15 new chunks for a single put (tree height ~3, generous for splits). Diff efficiency asserts get_count < 50% of full traversal get_count (5 changes in 1000 keys should skip most subtrees).
- **`Slow` tag:** Performance tests are marked `` `Slow `` in Alcotest to distinguish them from correctness tests.
- **Separate test file:** `test/test_perf.ml` keeps performance tests separate from correctness tests.

## Interface changes

```ocaml
(* store.mli — additions *)
val get_count : t -> int
val put_count : t -> int
val reset_stats : t -> unit
```

## Tests

### Store stats (in test_store.ml, `Quick`)

- Verify get_count/put_count increment correctly and reset_stats zeroes them.

### Structural sharing (in test_perf.ml, `Slow`)

- Build 1000-key tree with target_size:20.
- Put one new key.
- Assert put_count ≤ 15 (O(log n), not O(n)).

### Diff efficiency (in test_perf.ml, `Slow`)

- Build 1000-key tree with target_size:20.
- Apply 5 puts to produce a second tree.
- Fully traverse tree A via Tree.range, record full_traversal_gets.
- Diff tree A vs tree B, record diff_gets.
- Assert diff_gets < full_traversal_gets / 2.
