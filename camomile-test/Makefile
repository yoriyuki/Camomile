# Generated automatically from Makefile.in by configure.
#
# sample Makefile for Objective Caml
# Copyright (C) 2001 Jean-Christophe FILLIATRE
# 		2002, 2003 Yamagata Yoriyuki
# 
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License version 2, as published by the Free Software Foundation.
# 
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# 
# See the GNU Library General Public License version 2 for more details
# (enclosed in the file LGPL).

# where to install the binaries
prefix=/usr/local
exec_prefix=${prefix}
BINDIR=${exec_prefix}/bin

# where to install the man page
MANDIR=${prefix}/man

# other variables set by ./configure
OCAMLC   = ocamlfind ocamlc
OCAMLOPT = ocamlfind ocamlopt
OCAMLDEP = ocamldep
OCAMLLEX = ocamllex.opt
OCAMLYACC= ocamlyacc
OCAMLLIB = C:\OCaml\lib
OCAMLBEST= opt
OCAMLVERSION = 3.11
OCAMLWEB = ocamlweb

PACKAGES = -package unix -package str -package bigarray -package camomile -linkpkg
INCLUDES = -I lib -I tester-lib
BFLAGS = $(PACKAGES) -g $(INCLUDES)
OFLAGS = $(PACKAGES) $(INCLUDES)

# main target
#############

all: test.$(OCAMLBEST)

# bytecode and native-code compilation
######################################

CMO = lib/blender.cmo tester-lib/uStorageTest.cmo tester-lib/testUColJapanese.cmo
CMX = $(CMO:.cmo=.cmx)
TESTERS = $(wildcard tester/*.ml)

GENERATED = 

lib: $(CMO)
byte: $(TESTERS:.ml=.byte)
opt: $(TESTERS:.ml=.opt)

test.byte: $(TESTERS:.ml=.byte.log)
test.opt: $(TESTERS:.ml=.opt.log)

tester/%.log : tester/%
	$< --output tester 2>&1 | tee $<.log

tester/%.byte: $(CMO) tester/%.cmo lib/blenderMain.cmo
	$(OCAMLC) $(BFLAGS) -o $@  $(CMO) $(patsubst %.byte,%.cmo,$@) lib/blenderMain.cmo

tester/%.opt: $(CMX) tester/%.cmx lib/blenderMain.cmx
	$(OCAMLOPT) $(OFLAGS) -o $@  $(CMX) $(patsubst %.opt,%.cmx,$@) lib/blenderMain.cmx

# generic rules
###############

.SUFFIXES: .mli .ml .cmi .cmo .cmx .mll .mly

.mli.cmi:
	$(OCAMLC) $(BFLAGS) -c $<

.ml.cmo:
	$(OCAMLC) $(BFLAGS) -c $<

.ml.o:
	$(OCAMLOPT) $(OFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OFLAGS) -c $<

.mll.ml:
	$(OCAMLLEX) $<

.mly.ml:
	$(OCAMLYACC) -v $<

.mly.mli:
	$(OCAMLYACC) -v $<

# Emacs tags
############

tags:
	find . -name "*.ml*" | sort -r | xargs \
	etags "--regex=/let[ \t]+\([^ \t]+\)/\1/" \
	      "--regex=/let[ \t]+rec[ \t]+\([^ \t]+\)/\1/" \
	      "--regex=/and[ \t]+\([^ \t]+\)/\1/" \
	      "--regex=/type[ \t]+\([^ \t]+\)/\1/" \
              "--regex=/exception[ \t]+\([^ \t]+\)/\1/" \
	      "--regex=/val[ \t]+\([^ \t]+\)/\1/" \
	      "--regex=/module[ \t]+\([^ \t]+\)/\1/"

# myself

# clean
#######

clean::
	rm -f */*.cm[iox] */*.o *~
	rm -f $(GENERATED) parser.output
	rm -f $(TESTERS:.ml=.byte) $(TESTERS:.ml=.opt) 
	rm -f $(TESTERS:.ml=.byte.log) $(TESTERS:.ml=.opt.log)

# depend
########

.depend depend:: $(GENERATED)
	rm -f .depend
	$(OCAMLDEP) $(INCLUDES) lib/*.ml lib/*.mli > .depend
	$(OCAMLDEP) $(INCLUDES) tester-lib/*.ml tester-lib/*.mli >> .depend
	$(OCAMLDEP) $(INCLUDES) tester/*.ml tester/*.mli >> .depend

include .depend
