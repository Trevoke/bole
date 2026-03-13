# Bole

Content-addressed prolly tree library in OCaml.

## Known limitations

### Chunker mean undershoot

**Where:** `lib/chunker.ml` — the `feed` function's boundary detection formula.

**What:** The `Chunker` module uses a quadratic hazard ramp to decide chunk boundaries. Due to cumulative probability, the actual mean chunk size is approximately **0.5x to 0.8x** of the `target_size` parameter. A caller requesting `target_size:64` will get chunks averaging 32–51 entries.

**Why it matters:** When building a prolly tree (`Tree.build`), the `target_size` passed to the chunker must be set higher than the desired average chunk size to compensate.

**Future fix:** Replace the quadratic ramp with a Weibull CDF (shape K=4), which is what Dolt uses in production. The Weibull approach achieves ~0.91x of target and can be corrected exactly via the Gamma function. See `docs/bole-core-design-research.md` for background and `docs/plans/2026-03-12-prolly-tree-library-roadmap.md` for the deferred work list.
