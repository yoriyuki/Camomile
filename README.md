# Camomile

*This is an old version of Camommile.  The project was moved to [ocaml-community repo.](https://github.com/ocaml-community/Camomile).*

This is the version 1.0.1 release of Camomile library package. Camomile is a
Unicode library for ocaml. Camomile provides Unicode character type, UTF-8,
UTF-16, UTF-32 strings, conversion to/from about 200 encodings, collation and
locale-sensitive case mappings, and more.

The library is licensed under LGPL-2 with the common linking exception given to
OCaml packages. See [LICENSE.md](LICENSE.md)

## Installation

To build and install Camomile, you need [OCaml](https://ocaml.org) >= 4.02.3 [dune](https://dune.build) >= 1.0.0

To build the library, on the top directory do
```sh
$ dune build
$ dune install
```
Data files are put under /usr/local/share/camomile by default. The default can
be overridden by running
```sh
$ ocaml configure.ml --share=XXXX
```
then data files are
placed under XXXX/camomile.

You can uninstall the library by
```sh
$ dune uninstall
```

## Using libraries

### Wrapping

**camomile.cma** contains three top-level modules `CamomileLibrary`,
`CamomileLibraryDefault`, `CamomileLibraryDyn`.  Difference of three modules is
explained below.

**camomileLibrary.cma** contains `CamomileLibrary` alone. It is
useful if you want to dynamically load Camomile, since loading
`CamomileLibraryDefault`, `CamomileLibraryDyn causes` side effects such as loading
data files which could fail.

### Configuration

Camomile requires runtime configuration. Currently, you have to pass the
location of data files to Camomile.  In the future, more configuration variables
would be required.

Camomile's idea of configuration is "configuration by functors". Modules which
require configuration become functors parametrized by a module which contains
configuration variables. `CamomileLibrary.ConfigInt.Type` specifies the module
type of configuration parameters.  You can pass the configuration module to
individual modules' `Make` (as `UCol.Make`) or `Configure` functors (as
`CharEncoding.Configure`), or pass it to the whole-in-one functor
`CamomileLibrary.Make` and obtain configured modules.

Camomile provides two top-level modules `CamomileLibraryDefault` and
`CamomileLibraryDyn` which contains modules already configured.
`CamomileLibraryDefault` is configured by default values determined by configure.
Therefore it is suitable to use if you are using Camomile locally installed from
the source. `CamomileLibraryDyn` is deprecated and just an alias to
`CamomileLibraryDefault`.

### Individual modules.

See `CamomileLibrary.mli` file.

## Development

See https://github.com/yoriyuki/Camomile

## Author

You can contact the author by yoriyuki.y@gmail.com

## Acknowledgment

So many people are contributed to Camomile. See
https://github.com/yoriyuki/Camomile/graphs/contributors

Before GitHub becomes into existence...

- Peter Jolly provided CP932 conversion table.
- Kawakami Shigenobu contributed findlib support.
- Pierre Chambart contributed `StringPrep` module.
- Stanisław T. Findeisen pointed out the balancing bug of AVL-trees.
- Sylvain Le Gall provided dynamic configuration module `ConfigDyn.ml`.

Many people contributed bug fixes.
