2.0.0 (unreleased)
------------------

- Fix compilation with OCaml 5.

1.0.2 (2019-08-07)
------------------

- Use dune's include_subdirs (#72)

- Fix parallel build (#75)

- Add `(mode fallback)` to installConfig.ml (#76)

- Fix iana executable (#77)

- Generate opam files from dune. (#78)

1.0.1
-----

* Fix 4.02-4.04 compat
* Update 4.06 version


1.0.0
----------------

* CamomileLibraryDyn is deprecated
* Dependancies to cppo and bytes is removed
* Code cleanup

0.8.7
-----

* OCaml 4.02.3 is again supported

0.8.6
-----

* The build system is changed to jbuilder

* OCaml 4.0.6 is supported while the support for OCaml < 4.0.3 is droppped

* Bug fixes

0.8.5
-----

* The pull request https://github.com/yoriyuki/Camomile/pull/1 is merged.

* The traditional tar-ball release.

0.8.4
-----

* Scoping rule of "." in regular expressions is fixed to match the rule
  of Str of stdlib.  (Pointed out by Kawakami Shigenobu.)

* search_forward in URe and UReStr returns None if there is no matched
  substring, not raises Not_found.  (Pointed out by Kawakami Shigenobu.)

* IMap.domain and map_to_set fix (imported from batteries).

* Fix bug that ISet.(compare empty empty) = -1 (imported from batteries).

0.8.3
-----

* The bug that casefolding mapes lowercase letters to empty string is
  fixed.

0.8.2
-----

* File names of Data files which is produced and used by camomile are
  now properly escaped, to avoid security risk and more liberal use of
  aliases which contain "." or "/" etc.

* Charmap tables are updated to the recent ones of glibc.

0.8.1
-----

* Bugs affecting UMap.remove and USet.fold_range are fixed.  (Pointed
  out by Chris Kuklewicz.)

0.8.0
-----

* Reorganize the module structure : Make CamomileLibrary.Default to
  the new top-level module CamomileLibraryDefault, so that linking
  CamomileLibrary does not automatically cause to load the data files
  from hard-coded location.  Also, adding new CamomileLibraryDyn
  module, which is configured by environmental variables (See
  camomileDynConfig.mli).

* StringPrep module:  String preparation according to RFC3454,
  contributed by Pierre Chambart.

* Avl-Tree balanced bug fix, pointed out by StanisÅ‚aw T. Findeisen.

0.7.3
-----

* Windows (with Cygwin) is supported.

* Fix ISet, IMap merging bug
  http://sourceforge.net/tracker/?func=detail&aid=2881864&group_id=40603&atid=428416
  is fixed.

* Fix channel flush bug
  https://sourceforge.net/tracker/index.php?func=detail&aid=2809898&group_id=40603&atid=428416

* Several bug fixes of Makefile, including that cpp is not required to
  build any more.

0.7.2
-----

* License Changes : Exception clauses to LGPL similar to OCaml
  standard library are added.

0.7.1
-----

* Rename CamomileLibrary.Main.Camomile to CamomileLibrary.Default.Camomile

0.7.0
-----

* Initialization parameters for Camomile are given by Functor.

0.6.5
-----

* Collation rules without headers were not compiled. This version fix this
  problem. Collation of Scandinavia languages are now correctly processed.

0.6.4
-----

* Fix link error of the native code library

0.6.3
-----

* Support OCaml 3.09.0

* Remove unpack

0.6.2
-----

* Insert spaces before ] in the installation scripts of Makefile

0.6.1
-----

* Fix the bug that "get" methods of polymorphic input channels have the type 'a,
  which should be unit -> 'a.

* Fix a bug which causes flush methods of octet output channels only flush
  1024*n bytes at a time.

* Remove a superfluous check from configure.

0.6.0
-----

* Channel classes confirm common I/O class recommendation
(http://www.ocaml-programming.de/rec/IO-Classes.html) except
non-blocking I/O, which is not supported.

* Remove all C binding and related functions.

* Remove stdlib replacement introduced in 0.5.*

* UPervasives
  - utf8_*_channel are removed.
  - normalization mode are removed.

* UChar
  - UChar.is_printable is removed.
  - unsafe operations are removed.
  - UChar.int_of_uchar is renamed UChar.int_of
  - UChar.uchar_of_int is renamed UChar.of_int

* Locale
  - Locale.current_locale, Locale.set_locale are removed.

* CharEndocing
  - CharEncoding.enc_name is removed.
  - new classes:
	class CharEncoding.convert_uchar_input
	class CharEncoding.convert_uchar_output
	class CharEncoding.convert_input
	class CharEncoding.convert_output

0.5.3
-----

* Fix several bugs in the installation procedure.

* COPYING

* UTF16, UCS4 - improved performance

0.5.0
-----

* Stdlib replacement

* Search string by matching collation

* Binding to ISO C Locale.

* UChar.is_printable

* Locale.current_locale, Locale.set_locale

* CharEncoding.enc_name

* New encoding: iso_c_locale : the encoding specified by the current LC_CTYPE
  locale.

* New encoding: CP932

* Packed and unpacked libraries.

0.4.2
-----

* Add bigarray to the dependency in META.
* Fix a bug in input_line function of ULine.
* Fix a XArray.add_array bug, which affects XString.add_text,
UText.Buf.add_string.
* Make the collator a bit fast.

0.4.1
-----

* CharEndocing
 - The encodings can be referred by IANA names.

* UCol
 - Japanese collation is fully supported (HiraganaQ).
 - Bugs which mainly affect Japanese collation are fixed.
 - generates more compact sort keys.

* ULine
 - Line I/O, and conversion of line separators.

* Data tables are now held through weak pointers, so that they can be
freed during GC.

* All files in charmap directory become optional.  Previously, the build
failed if several charmaps were missing.  In addition, you can remove any
file in the $DATADIR/charmaps and $DATADIR/mappings without causing
run time error.  Of course, this makes some encoding dysfunctional.
Such encodings are treated as non-existent.

0.4.0
-----

* Renamed to Camomile (previously Base)

* USet (Unicode character sets), UCharTbl (fast lookup tables)

* SubText

* URe, UReStr (Regular Expression) are added.

* UTF16, UCS4 (new Unicode string types) are added.

* findlib support

* ocamldoc support

0.3.1
-----

* CharEncoding: Bug fixes for ISO-2022-*

* CharEncoding: Interface for automatic detection of encodings.

* CharEncoding: GB18030 support

* CharEncoding: Improvement of internal data structure (using less space)

* UCol: Incremental comparison

* Performance improvement of collation rule compiler.


0.3.0
-----

* Functor design: API taking Unicode strings becomes functors over Unicode
string implementation. For Unicode strings, UText.t (abstract data type,
internally integer array) and UTF8.t (normal ocaml string with UTF8 encoding)
are currently provided, but suppling another implementation (like wch) should be
easy. For this, API is almost completely revised.

* The locale can be specified for case mapping and string comparison.
