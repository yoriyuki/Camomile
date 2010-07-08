Camomile 0.8.1 (beta):

This is a beta release of Camomile library package.  Camomile is a
comprehensive Unicode library for ocaml.  Camomile provides Unicode
character type, UTF-8, UTF-16, UTF-32 strings, conversion to/from
about 200 encodings, collation and locale-sensitive case mappings, and
more.

Copyright 2001, 2002, 2003, 2004, 2005, 2006, 2009, 2010 Yoriyuki
Yamagata 

This library is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

As a special exception to the GNU Library General Public License, you
may link, statically or dynamically, a "work that uses this library"
with a publicly distributed version of this library to produce an
executable file containing portions of this library, and distribute
that executable file under terms of your choice, without any of the
additional requirements listed in clause 6 of the GNU Library General
Public License. By "a publicly distributed version of this library", we
mean either the unmodified Library as distributed by INRIA, or a
modified version of this library that is distributed under the
conditions defined in clause 3 of the GNU Library General Public
License. This exception does not however invalidate any other reasons
why the executable file might be covered by the GNU Library General
Public License.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

The GNU Lesser General Public License (LGPL) is contained in the file COPYING.

The following files are authored by different persons.  For their
licenses, see the relevant notice of each file or directory.

camomileDynConfig.ml* : Sylvain Le Gall
	Licensed by LGPL	      

internal/stringPrep_data.ml*, public/stringPrep.ml*,
tools/camomilestringprep.ml parse_age.ml, many changes to Makefile.in
public/uCharInfo.ml* : Pierre Chambart
	Licensed by LGPL

configure.in Makefile.in : Jean-Christophe FILLIATRE
	Licensed by LGPL

locales : International Business Machines : 
	derived from the ICU package.  see locales/licence.html
mappings : Free Software Foundation : 
	derived from glibc.  Licensed by LGPL
	(CP932 has a different origin.  See the preamble of the file CP932)
unidata : Unicode Inc. : see unidata/README

1. Installation

The library is designed to work all platform supported by ocaml, but
currently the build procedure requires GNU tools (GNU coreutils, Make,
grep, sh).
In the top of the source directory, do the following.
	$ ./configure
	$ make
	$ sudo make install

The installation directories depend on whether you have findlib, which
is automatically detected.  If you do not have findlib, Library files
(camomile.cma, camomile.cmxa and other auxiliary files) and
interface files (camomileLibrary.cmi) are installed to ocaml standard
library directory.  If you have findlib, these files are governed by
findlib and installed as a package named "camomile".  For either
cases, data files are put under /usr/local/share/camomile by default.
If you give the configure option as
	$ ./configure --datadir=XXXX 
data files are placed under XXXX/camomile.

You can uninstall the library by
	$ sudo make uninstall

If you have ocamldoc,
	$ make dochtml
creates dochtml directory in the source directory, and HTML
documentation.

If you use MinGW port of OCaml and put OCaml stuff into the folder
C:/OCaml, then specify prefix when invoking configure.  
	  $ configure --prefix=C:/OCaml 
The prefix must be in the MSDOS file format and separators of path
must be /, not \.

2. Using libraries

2.1 Packing

"camomile.cma" contains three top-level modules CamomileLibrary,
CamomileLibraryDefault, CamomileLibraryDyn.  Difference of three
modules is explained below.

"camomileLibrary.cma" contains CamomileLibrary alone.
"camomileLibrary.cma" is useful if you want to dynamically load
Camomile, since loading CamomileLibraryDefault, CamomileLibraryDyn
causes side effects such as loading data files which could fail.

2.2 Configuration

Camomile requires runtime configuration.  Currently, you have to pass
the location of data files to Camomile.  In the future, more
configuration variables would be required.

Camomile's idea of configuration is "configuration by functors".
Modules which require configuration become functors parametrized by a
module which contains configuration variables.
CamomileLibrary.ConfigInt.Type specifies the module type of
configuration parameters.  You can pass the configuration module to
individual modules' Make (as UCol.Make) or Configure functors (as
CharEncoding.Configure), or pass it to the whole-in-one functor
CamomileLibrary.Make and obtain configured modules.

Camomile provides two top-level modules CamomileLibraryDefault and
which contains modules already configured.  CamomileLibraryDefault is
configured by default values determined by configure.  Therefore it is
suitable to use if you are using Camomile locally installed from the
source.  CamomileLibraryDyn is configured at runtime using
environmental variables.  Therefore it is suitable to use if you plan
to relocate data files.  Note that data files are just results of
output_value, and loaded by input_value without *Any Check of Their
Validity*.  Therefore, you should not use CamomileLibraryDyn if
a malicious user can change the values of environmental variables.

2.3 Individual modules.

See CamomileLibrary.mli file.  Also see camomileDynConfig.mli.

2.4 Migration from 0.7.X

If you are just using CamomileLibrary.Default.Camomile module, replace
it with CamomileLibraryDefault.Camomile (no dot between
CamomileLibrary and Default).

If you are using CamomileLibrary.Main.Make, use CamomileLibrary.Make
instead. 

3. Development

You can checkout the development version of Camomile by the following
command. 
    $ svn co https://camomile.svn.sourceforge.net/svnroot/camomile camomile  

To build the development version of Camomile, you need autoconf and cppo
pre-processor.  Then, enter the working directory and do the
following.
    $ autoconf
    $ ./configure --enable-debug ...
    $ make
    $ sudo make install

4. Author

You can contact the author by yori@sourceforge.net

5. Acknowledgment

Peter Jolly provided CP932 conversion table.  Kawakami Shigenobu
contributed findlib support.  Pierre Chambart contributed StringPrep
module.  Stanisław T. Findeisen pointed out the balancing bug of
AVL-trees.  Sylvain Le Gall provided dynamic configuration module
ConfigDyn.ml.

Many people contributed bug fixes.

