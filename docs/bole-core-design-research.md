# The prolly tree is the whole trick

**The simplest diffable relational database reduces to one core insight: use a prolly tree — a B-tree with content-defined chunk boundaries — as your table index, keyed by primary key.** This single data structure simultaneously gives you O(log n) queries, O(d log n) diffs, structural sharing across versions, and deterministic structure from content. The complete system is: *content-addressed prolly trees as table storage + a commit DAG for history + cell-level three-way merge over parallel diff streams.* That's it. Everything else is engineering.

This mirrors git's elegant core — content-addressable blob store, Merkle tree of trees, three-way merge — but adapted for sorted, keyed, relational data. Where git's fundamental object is a blob addressed by its hash, this system's fundamental object is a chunked sorted map addressed by its root hash. The design space has been explored by Noms (~2015), refined by Dolt (2018–present), and informed by academic work on Merkle-CRDTs, categorical patch theory, and relational CRDTs. The architecture is now well-understood enough to describe precisely.

## Why B-trees can't diff and Merkle trees can't query

The core problem is that traditional database indexes (B-trees) and traditional version-control structures (Merkle trees) each solve half the problem. A B-tree gives O(log n) lookups and range scans but its structure depends on insertion order — two B-trees containing identical data can have completely different node layouts. Diffing two B-trees requires a full O(n) scan. A Merkle tree gives O(d log n) diffs via hash comparison but doesn't support efficient key lookups or range scans.

A **prolly tree** (probabilistic B-tree) fuses both properties through one mechanism: content-defined chunk boundaries. The construction is straightforward. Sort all key-value pairs by key. Walk the sorted sequence, and after each entry, evaluate a hash function on the key. If the hash falls below a threshold, declare a chunk boundary. Hash each chunk's contents to get its content-address. Build a tree of internal nodes the same way — each internal entry maps a chunk's maximum key to its content-address, and internal nodes are themselves chunked by the same boundary function. The result is a B+-tree-shaped structure where **the tree's topology is determined entirely by the data's content**, not by mutation history.

This property — called **history independence** — is what makes everything work. Insert rows in any order, delete and re-insert, rebuild from scratch: you always get the same tree. Two trees with identical contents have identical root hashes. Two trees differing by d rows share all but approximately d · log_k(n) nodes (where k is the average chunk branching factor, typically ~100 for 4KB chunks). The diff algorithm is a parallel descent: compare root hashes; if equal, skip the entire subtree; if different, descend and compare children. Only nodes containing actual differences are ever read.

Dolt's second-generation chunking algorithm improved on Noms' original design in two important ways. First, it hashes **only keys**, not values — since keys are unique, they provide sufficient entropy, and this means updating a fixed-width column value never shifts chunk boundaries. Second, it uses a dynamic probability function where the likelihood of a boundary *increases with chunk size*, producing a quasi-Gaussian chunk-size distribution instead of Noms' geometric distribution (which suffered from high variance — too many tiny chunks, some enormous ones).

## Three primitives compose into a complete system

The full architecture requires exactly three layers that compose cleanly:

**Layer 1 — Chunk store.** A content-addressable key-value store mapping hash → bytes. This is the analog of git's object store. Chunks are immutable, append-only, typically ~4KB compressed with Snappy. Dolt's implementation uses append-only "table files" with binary-searchable indexes, plus a write-ahead journal for ACID transactions. Any key-value store with content-addressing works here.

**Layer 2 — Prolly tree maps.** Built on the chunk store, a prolly tree implements `SortedMap<KeyTuple, ValueTuple>` where both keys and values are serialized tuples. A **table** is a prolly tree keyed by primary key, with non-key columns as the value. A **secondary index** is a separate prolly tree keyed by (index columns, primary key) pointing back to the primary key. A **database state** is a small map from table names to (schema hash, data tree root hash) pairs. A **commit** is an object containing a root-hash pointer to a database state, parent commit hashes, and metadata — forming a Merkle DAG. Branching is O(1): create a new named pointer to an existing commit hash.

**Layer 3 — Three-way cell-level merge.** Given two branch tips and their nearest common ancestor (found by BFS on the commit DAG), compute two diffs: `Diff(ancestor, left)` and `Diff(ancestor, right)`. Each diff is a stream of `(key, old_value, new_value, type)` tuples produced by the prolly tree diff algorithm, emitted in primary-key order. Merge these two sorted streams:

- Key appears in only one diff → apply that change, no conflict.
- Key appears in both diffs with the same change → no conflict, keep it.
- Key appears in both diffs with different changes → examine cell-level: if the two branches modified *different columns* of the same row, merge cleanly by applying both column changes to the ancestor row. If both modified the *same column* to different values, or one deleted the row while the other modified it → **conflict**.

Conflicts are stored in a separate prolly tree alongside the table, keyed by primary key, containing the base/left/right versions. Resolution is explicit: the user chooses or writes a reconciled value. This is exactly analogous to git's conflict markers, but structured.

The one-sentence summary: **Tables are prolly trees keyed by primary key in a content-addressed chunk store, diffing is a parallel hash-comparison descent, and merging is cell-level three-way reconciliation of two such diff streams.**

## What the existing projects teach us

**Dolt** is the existence proof that this architecture works at scale. It's a MySQL-compatible SQL database (using the `go-mysql-server` engine) with full git semantics — branch, commit, merge, clone, push, pull. It can run embedded in Go applications via `github.com/dolthub/driver`, or as a MySQL-protocol server. Tables are prolly trees; commits form a Merkle DAG; merges are cell-level three-way. Dolt achieved 1.0 in May 2023 and handles production workloads. Its storage overhead is roughly **2× MySQL** for typical versioned workloads, with branching as O(1) and diffs scaling with change size, not data size.

**Noms** (2015–2018) was the research prototype that invented prolly trees and proved the concept. It was schema-less and document-oriented — a content-addressed "decentralized database" with git-like semantics. Noms' key limitation was its self-describing type system (type information encoded in every serialized value), which made it too slow and storage-heavy for relational workloads. Dolt forked Noms, stripped the type system, added SQL, and redesigned the chunking algorithm.

**TerminusDB** takes a different path: immutable append-only layers of RDF triples with delta encoding. Each transaction creates a new layer; deletions are masks. This gives git-like semantics for graph data, but the triple-based model creates impedance mismatch for relational workloads, and deep histories (>1000 commits) degrade performance without periodic squashing.

**Irmin** (OCaml, MirageOS) is the most principled library-level design: a content-addressable block store with pluggable backends (including git-compatible) and **user-defined merge functions** per data type. Irmin requires developers to specify how their types merge — counters get additive merge, registers get LWW, custom types get custom merge. It's a toolkit for building mergeable stores, not a database. Its mathematical foundation draws on inverse semigroups (from Darcs' patch theory) and pushouts in categories.

**cr-sqlite** bolts CRDT semantics onto SQLite via a runtime extension. Tables are upgraded to "Conflict-free Replicated Relations" where each column becomes an LWW register (or counter). Changes are tracked in metadata tables and synced as column-level changesets. It works but cannot maintain relational invariants — **foreign keys, uniqueness constraints, and CHECK constraints can all be violated by concurrent operations**. ElectricSQL's "Rich-CRDTs" address this with compensations (re-creating referenced objects that were concurrently deleted) and escrow reservations (pre-allocating uniqueness tokens), but add substantial complexity.

## CRDTs versus three-way merge: a fundamental fork

Given Aldric's interest in CRDT-based version control, the CRDT vs. three-way merge distinction deserves careful treatment. These are genuinely different approaches with different guarantees, and the choice has deep architectural consequences.

**Three-way merge** (git-style, used by Dolt) requires a common ancestor and produces either a clean merge or explicit conflicts. Conflicts are surfaced to the user for resolution. This preserves developer intent — no information is silently discarded. The downside: you need access to the ancestor, and conflicts require human intervention. The upside: complex relational invariants can be validated post-merge.

**CRDT merge** requires no ancestor and always succeeds — convergence is guaranteed by mathematical properties (the merge function forms a join-semilattice: commutative, associative, idempotent). The downside is that "always succeeds" means information can be silently lost. An LWW register resolving two concurrent writes to the same cell discards one write with no notification. The deeper problem: **relational constraints (foreign keys, uniqueness) cannot be maintained by local-only CRDT operations** without either coordination or post-hoc repair.

The Synql paper (Ignat, Elvinger, Ba; DAIS 2024) offers the most rigorous treatment: separate the CRDT merge layer (which guarantees Strong Eventual Consistency) from a deterministic constraint-enforcement layer that derives a valid relational state from the replicated CRDT state. This is elegant but means the "true" database state is a computed view over the CRDT state, not the CRDT state itself.

**Pijul's patch theory** (based on Mimram and Di Giusto's categorical theory of patches) offers an interesting middle ground: patches are morphisms in a category, merges are pushouts, and a branch is an *unordered set of patches* rather than an ordered sequence. This makes merge commutative and associative by construction — essentially CRDT-like — while still surfacing conflicts when pushouts don't exist cleanly. Pijul's "pristine" (current file state) is itself a CRDT. However, Pijul operates on lines of text, not relational rows, and extending its patch theory to structured data with constraints is an open problem.

For a diffable relational database specifically, **three-way merge is the more natural fit** because relational data has strong invariants (primary key uniqueness, foreign key integrity, CHECK constraints) that CRDTs cannot easily maintain. The prolly tree diff provides the "diff" side efficiently, and cell-level three-way merge provides conflict detection with no silent information loss.

## Schema evolution remains the hard unsolved problem

Every project in this space handles data versioning reasonably well. **Schema versioning across branches is where they all struggle.** When branch A adds a column and branch B renames a different column, merging the schema changes is tractable. When branch A adds a NOT NULL column (requiring data migration) while branch B drops the table entirely, no automated system handles this well.

Dolt merges schemas by treating DDL operations as diffs and checking for cell-level conflicts in schema metadata — but complex conflicts (contradictory type changes, structural reorganizations) require choosing one side entirely. PlanetScale's schemadiff tests commutativity: if `diff1(diff2(base)) == diff2(diff1(base))`, no conflict; otherwise, conflict.

The most theoretically ambitious approach is Edwards and Petricek's "Baseline" (2025), which treats schema changes as first-class operations in the version history — column adds, renames, table splits are all recorded as operations that can be diffed and merged alongside data changes, using generalized operational transformation (Project/Retract functions). This is powerful but unproven in practice.

Cambria's bidirectional lenses offer a complementary idea: schema migrations as composable, reversible transformations. A lens from schema A to schema B can automatically translate data and operations in both directions. But Cambria is limited to JSON documents and cannot express all relational schema changes (particularly those involving foreign keys or constraints).

**The pragmatic answer**: require primary keys on all tables, version schema separately from data, and treat conflicting schema changes as mandatory-manual-resolution conflicts. This is what Dolt does, and it works.

## Trade-offs that shape the design space

**Row-oriented storage is strictly better for diffability.** Prolly trees store sorted key-value pairs, and row-oriented storage means each row is one entry — a single-row change modifies one leaf chunk plus the path to root (~log n nodes, ~4KB each). Column-oriented storage would require N separate prolly trees for N columns, and a single-row insert would modify all N trees. The write amplification is unacceptable for versioned workloads. Dolt is row-oriented for this reason.

**Primary keys are non-negotiable.** Without a stable row identifier, "row X was modified" is undefined — you'd need heuristic matching between row sets, which is both expensive and ambiguous. Every diffable database design requires primary keys. Dolt handles keyless tables by treating the entire row as the key with a duplicate count as the value, but this is a compromise that degrades merge quality.

**Git-compatible storage (storing files that git can track) fundamentally cannot achieve semantic diffing.** Git's diff operates on lines of text. Even with sqlite-diffable (which serializes each table as newline-delimited JSON), you get line-level diffs with no understanding of cell-level changes, no schema-aware merging, and O(n) diff cost regardless of change size. Simon Willison's sqlite-diffable is useful for human-readable backups in git, but it's not a database versioning system. **Git-like storage (own versioning layer using the same concepts) is necessary** for semantic, efficient versioning.

**Storage overhead is modest.** Content-addressing adds ~20 bytes of hash per chunk. Structural sharing across versions means unchanged data is stored exactly once. Dolt reports roughly 2× the storage of an unversioned MySQL database for typical workloads — a small price for full version history. The chunk journal + table file compaction approach provides ACID guarantees with reasonable write amplification.

## Conclusion: the design in two sentences

The entire design space for a diffable embedded relational database converges on a single architectural pattern, independently discovered by the Noms/Dolt lineage and anticipated by the Merkle-CRDT and categorical patch theory literature:

**Store each table as a prolly tree (a B-tree with content-defined chunk boundaries in a content-addressed block store) keyed by primary key. Version via a commit DAG over root hashes. Diff by parallel descent comparing chunk hashes. Merge by cell-level three-way reconciliation of two diff streams against their common ancestor.**

The prolly tree is the essential innovation — it's the only known data structure that simultaneously provides B-tree query performance, Merkle-tree diffing efficiency, and history-independent deterministic structure. Everything else follows from this primitive. A minimal implementation needs: (1) a content-addressed chunk store, (2) a prolly tree implementation (the chunking function is ~50 lines; the tree operations are standard B-tree algorithms adapted for immutable nodes), (3) a commit object format, and (4) a merge function that streams two diffs and reconciles at cell granularity.

The remaining open frontier is not the core versioning machinery — that's solved — but schema evolution across branches, and the choice between three-way merge (which surfaces conflicts for human resolution and preserves relational invariants) versus CRDT merge (which guarantees convergence but weakens invariants). For an embedded relational database where correctness matters, three-way merge is the right default. CRDTs are the right choice when availability under partition matters more than constraint enforcement. A hybrid — CRDT-style merge for known-safe operations, explicit conflicts for everything else — remains the most interesting unexplored point in the design space.
