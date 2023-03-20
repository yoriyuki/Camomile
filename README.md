# Camomile

This is the version 2.0.0 release of Camomile library package. Camomile is a
Unicode library for ocaml. Camomile provides Unicode character type, UTF-8,
UTF-16, UTF-32 strings, conversion to/from about 200 encodings, collation and
locale-sensitive case mappings, and more.

The library is licensed under LGPL-2 with the common linking exception given to
OCaml packages. See [LICENSE.md](LICENSE.md)

## Installation

The recommended way to install Camomile is via the [opam](https://opam.ocaml.org/) package manager. With
the `opam` binary installed and configured, all you should have to do is:

```sh
$ opam install camomile
````

Otherwise, you might want to see if your distribution has a package available for it.

## Building

For developers and testers, [dune](https://github.com/ocaml/dune) is used to build the library:

```sh
$ dune build
```

This includes a default library configured with paths internals to `dune` where its
shared files are stored.

The `Camomile` API provides a functor-base configuration interface that allows to
define custom libraries with shared files located at a different location.

If you are building for packaging purposes and need to place shared files in a specific
location for the default library, you can use environment variables at build-time:

```sh
$ CAMOMILE_CONFIG=env CAMOMILE_PREFIX=/usr dune build
```
in which case, the library will look for those files at `/usr/share/camomile`

Please note that `dune install` installs the file under the same local directory regardless of this option.
This option is meant to be used when packaging the library for distributions and binary packages such
as debian or ubuntu packages, RPM package etc.

## Using libraries

### Configuration

Camomile requires a runtime configuration to be able to locate its data files.

Camomile's idea of configuration is "configuration by functors". Modules which
require configuration become functors parametrized by a module which contains
configuration variables. `Camomile.Config.Type` specifies the module
type of configuration parameters.  You can pass the configuration module to
individual modules' `Make` (as `UCol.Make`) or `Configure` functors (as
`CharEncoding.Configure`), or pass it to the whole-in-one functor
`Camomile.Make` and obtain configured modules.

Camomile provides two top-level modules which contains modules already configured.
Therefore it is suitable to use if you are using Camomile locally installed from
the source.

### Individual modules.

See `camomile.ml` file.

## Development

See https://github.com/savonet/Camomile

## Author

Camomile is currently maintained by Romain Beauxis <romain.beauxis@gmail.co>
and was forked over from the original project with permission from @yoriyuki

However, the project is still in needs of active contributors. Please file pull
requests and more!

## Acknowledgment

So many people are contributed to Camomile. See
https://github.com/savonet/Camomile/graphs/contributors

Before GitHub becomes into existence...

- Peter Jolly provided CP932 conversion table.
- Kawakami Shigenobu contributed findlib support.
- Pierre Chambart contributed `StringPrep` module.
- Stanisław T. Findeisen pointed out the balancing bug of AVL-trees.
- Sylvain Le Gall provided dynamic configuration module `ConfigDyn.ml`.

Many people contributed bug fixes.
