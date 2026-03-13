(lang dune 3.17)

(name bole)

(generate_opam_files true)

(package
 (name bole)
 (synopsis "Content-addressed prolly tree library")
 (depends
  (ocaml (>= 5.2))
  (dune (>= 3.17))
  (digestif (>= 1.2.0))
  (alcotest (and (>= 1.8.0) :with-test))
  (qcheck (and (>= 0.22) :with-test))
  (qcheck-alcotest (and (>= 0.22) :with-test))))
