(lang dune 3.14)

(name lab3)

(package
 (name lab3)
 (depends ocaml dune zanuda ocamlformat ounit2 base stdio cmdliner angstrom)
)

(generate_opam_files true)

(source
 (github Demid0ITMO/s5-fp-lab3))

(authors "Ryazanov Demid")
