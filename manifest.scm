;; manifest.scm — Guix development environment for bole
;; Usage: guix shell -m manifest.scm

(specifications->manifest
  '("ocaml"
    "dune"
    "opam"
    "gcc-toolchain"
    "pkg-config"))
