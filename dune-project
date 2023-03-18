(lang dune 3.4)
(name camomile)
(version 2.0)

(generate_opam_files true)
(license "LGPL-2.0-or-later WITH OCaml-LGPL-linking-exception")
(maintainers "yoriyuki.y@gmail.com")
(authors "Yoriyuki Yamagata")
(implicit_transitive_deps false)
(source (github yoriyuki/Camomile))
(documentation "https://yoriyuki.github.io/Camomile/")

(package
 (name camomile)
 (synopsis "A Unicode library")
 (description "\
Camomile is a Unicode library for OCaml. Camomile provides Unicode character
type, UTF-8, UTF-16, UTF-32 strings, conversion to/from about 200 encodings,
collation and locale-sensitive case mappings, and more. The library is currently
designed for Unicode Standard 3.2.")
 (depends
  dune
  camlp-streams
  (ocaml (>= 4.13))))
