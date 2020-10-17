# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile

PACKAGEPREFIX=

DESTDIR=
RM=rm

LAUNCH=env TOP=$(TOP) $(TOP)/tools/LAUNCH
OCAMLFIND=$(LAUNCH) ocamlfind
NOT_OCAMLFIND=$(LAUNCH) not-ocamlfind
MKCAMLP5=$(LAUNCH) mkcamlp5
PACKAGES := event

all: \
	test_read_file.byte test_read_file.opt \
	test_refill_file.byte test_refill_file.opt \
	test_read_buffer.byte test_read_buffer.opt \
	test_ab.byte test_ab.opt \


test: all
	./test_read_file.opt /dev/zero 4096 10240000
	./test_read_file.opt /dev/zero 409600 10240000
	./test_refill_file.opt /dev/zero 4096 10240000
	./test_refill_file.opt /dev/zero 409600 10240000
	./test_read_buffer.opt 4096 1024000
	./test_read_buffer.opt 262144 1024000
	./test_ab.opt direct 262144 102400000
	./test_ab.opt kont 262144 102400000


% : %.byte
	mkdir -p _build && ./$<

test_read_file.byte: test_read_file.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) -package $(PACKAGES) \
	-linkpkg -linkall $^ -o $@

test_read_file.opt: test_read_file.cmx
	$(OCAMLFIND) ocamlopt $(DEBUG) $(WARNERR) $(OCAMLOPTFLAGS) -package $(PACKAGES) \
	-linkpkg -linkall $^ -o $@

test_refill_file.byte: test_refill_file.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) -package $(PACKAGES) \
	-linkpkg -linkall $^ -o $@

test_refill_file.opt: test_refill_file.cmx
	$(OCAMLFIND) ocamlopt $(DEBUG) $(WARNERR) $(OCAMLOPTFLAGS) -package $(PACKAGES) \
	-linkpkg -linkall $^ -o $@

test_read_buffer.byte: test_read_buffer.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) -package $(PACKAGES) \
	-linkpkg -linkall $^ -o $@

test_read_buffer.opt: test_read_buffer.cmx
	$(OCAMLFIND) ocamlopt $(DEBUG) $(WARNERR) $(OCAMLOPTFLAGS) -package $(PACKAGES) \
	-linkpkg -linkall $^ -o $@

test_ab.byte: test_ab.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) -package $(PACKAGES) \
	-linkpkg -linkall $^ -o $@

test_ab.opt: test_ab.cmx
	$(OCAMLFIND) ocamlopt $(DEBUG) $(WARNERR) $(OCAMLOPTFLAGS) -package $(PACKAGES) \
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
