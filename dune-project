(lang dune 3.4)
(using dune_site 0.1)

(name camomile)
(version 2.0)

(generate_opam_files true)
(license "LGPL-2.0-or-later WITH OCaml-LGPL-linking-exception")
(maintainers "romain.beauxis@gmail.com")
(authors "Yoriyuki Yamagata")
(source (github savonet/Camomile))
(documentation "https://savonet.github.io/Camomile/")

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
  dune-site
  camlp-streams
  (ocaml (>= 4.13)))
  (sites (share database) (share mappings) (share charmaps) (share locales)))
