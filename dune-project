(lang dune 3.17)

(name mini_ml)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Vivien GACHET <vivien.gachet@ens-lyon.fr")

(maintainers "Vivien GACHET <vivien.gachet@ens-lyon.fr")

(license LICENSE)

(documentation https://url/to/documentation)

(using menhir 2.1)

(package
 (name mini_ml)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
