(lang dune 3.9)
(name pdb)
(generate_opam_files true)
(cram enable)

(license BSD3)
(maintainers "Tim McGilchrist <timmcgil@gmail.com>")
(authors "Tim McGilchrist <timmcgil@gmail.com>")
(source (github tmcgilchrist/ocaml-pdb))

(package
 (name pdb)
 (tags (pdb codeview debug windows))
 (depends
  (ocaml (>= 4.14))
  object
  integers
  (cmdliner :with-test)
  (alcotest :with-test)
  (qcheck-core :with-test)
  (qcheck-alcotest :with-test)
  (ppx_import :with-test)
  (ppx_deriving_qcheck :with-test)
  (patdiff :with-dev-setup)
  (ocamlformat (and :with-dev-setup (= 0.28.1))))
 (synopsis
  "A library for reading and writing PDB/CodeView debugging information.")
 (description
  "A library for reading and writing PDB (Program Database) and CodeView debugging information used on Windows."))
