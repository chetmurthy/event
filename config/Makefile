# Makefile,v
# Copyright (c) INRIA 2007-2017

RM=rm

OCAMLFIND=ocamlfind
NOT_OCAMLFIND=not-ocamlfind
OCAMLTOPLEVEL=ocaml
INCLUDES=
OCAMLCFLAGS=$(INCLUDES)
OCAMLOPTFLAGS=-inline 200
PACKAGES=

export PERL5LIB := $(TOP):$(PERL5LIB)

top: all

.mli.cmi:
	$(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR)  $(OCAMLCFLAGS) -package $(PACKAGES) -c $<

.ml.cmo:
	$(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR)  $(OCAMLCFLAGS) -package $(PACKAGES) -c $<

.ml.cmx:
	$(OCAMLFIND) ocamlopt $(DEBUG) $(WARNERR)  $(OCAMLCFLAGS) $(OCAMLOPTFLAGS) -package $(PACKAGES) -c $<

.SUFFIXES: .cmo .cmi .ml .mli .cmx .cma .cmxa .cmt .cmti

clean::
	$(RM) -rf *.cm* *.o *.a *.byte *.opt *.log *.cache *.ppo *.ppo.ml _build/* .cmt *.odoc

realclean:: clean
