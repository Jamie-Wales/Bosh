(lang dune 3.15)

(name shell)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name shell)
 (synopsis "A posix shell implementation")
 (description "An ocaml description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

