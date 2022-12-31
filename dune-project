(lang dune 2.2)
(name hsplay)
(generate_opam_files true)

(license "MIT")
(authors "John Hester")
(maintainers "John Hester")

(version 1.5.1)

(package
 (name hsplay)
 (synopsis "ocaml splay tree implementation")
 (description "")
 (depends
  ppx_deriving
  (containers (>= 3.0))
  (alcotest :with-test)
  (containers-data (>= 3.0))
  (dune (>= 2.0))
  (zarith (>= 1.9.1))
  (ocamlgraph (>= 2.0.0))))
