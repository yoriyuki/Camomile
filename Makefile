INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

all:
	dune build @install

install:
	dune install $(INSTALL_ARGS)

uninstall:
	dune uninstall $(INSTALL_ARGS)

reinstall: uninstall reinstall

test:
	dune runtest

clean:
	dune clean

all-supported-ocaml-versions:
	dune build --workspace dune-workspace.dev

.PHONY: all-supported-ocaml-versions all install uninstall reinstall test clean
