# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=$(WD)/..
include $(TOP)/config/Makefile

PACKAGEPREFIX=

DESTDIR=
RM=rm

LAUNCH=env TOP=$(TOP) $(TOP)/tools/LAUNCH
OCAMLFIND=$(LAUNCH) ocamlfind
NOT_OCAMLFIND=$(LAUNCH) not-ocamlfind
MKCAMLP5=$(LAUNCH) mkcamlp5
PACKAGES := event

TESTS= \
	test_read_file.byte test_read_file.opt \

all: $(TESTS)

test: $(TESTS:.byte=)

% : %.byte
	mkdir -p _build && ./$<

test_read_file.byte: test_read_file.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) -package $(PACKAGES) \
	-linkpkg -linkall $^ -o $@

test_read_file.opt: test_read_file.cmx
	$(OCAMLFIND) ocamlopt $(DEBUG) $(WARNERR) -package $(PACKAGES) \
	-linkpkg -linkall $^ -o $@

clean::
	rm -f  *.ppo.*

EXTERNAL := $(shell $(OCAMLFIND) query -predicates byte -format '%m' $(ALL_PACKAGES) | grep local-install)
$(CMO): $(EXTERNAL)

depend::
	echo "$(CMO): $(EXTERNAL)" > .depend.NEW
	$(OCAMLFIND) ocamldep $(DEBUG) $(WARNERR) $(OCAMLCFLAGS) -package $(PACKAGES) \
	*.ml \
	>> .depend.NEW
	mv .depend.NEW .depend

-include .depend
