(lang dune 2.8)

(using menhir 2.1)

(name npuzzle)

(authors "Dario Pinto <laradiopotin@gmail.com>")

(maintainers "Dario Pinto <laradiopotin@gmail.com>")

(source
 (github RadioPotin/npuzzle42))

(generate_opam_files true)

(package
 (name npuzzle)
 (synopsis "A Npuzzle solver")
 (description "An Implementation of the A* algorithm.")
 (depends
  (ocaml
   (>= 4.08))
  (dune
   (>= 2.8))
  (menhir
   (>= 2.1))
  (bisect_ppx
   (and
    :with-test
    (>= 2.6)
    :dev))))
