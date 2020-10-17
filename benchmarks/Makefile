# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile


all: async_read_file.opt async_read_buffer.opt \
	lwt_read_file.opt lwt_read_buffer.opt \
	direct_read_file.opt direct_read_buffer.opt

F=/dev/zero
N=1048576
NLONG=10485760
NLONGLONG=104857600
BUFLEN=65536
test: all
	@echo "==== read file buffer $(BUFLEN) ===="
	./async_read_file.opt $F $(BUFLEN) $(NLONG)
	./lwt_read_file.opt $F $(BUFLEN) $(NLONGLONG)
	../tests/test_read_file.opt $F $(BUFLEN) $(NLONGLONG)
	@echo "==== read file buffer 16 ===="
	./async_read_file.opt $F 16 $N
	./lwt_read_file.opt $F 16 $(NLONG)
	../tests/test_read_file.opt $F 16 $(NLONGLONG)
	@echo "==== just buffer 1024 ===="
	./async_read_buffer.opt $F $(NLONGLONG)
	./lwt_read_buffer.opt $F $(NLONGLONG)
	../tests/test_read_buffer.opt 1024 $(NLONGLONG)
	./direct_read_buffer.opt $F $(NLONGLONG)
	@echo "==== misc ===="
	./direct_read_file.opt $F $(NLONGLONG)
	../tests/test_read_buffer.opt $(BUFLEN) $(NLONGLONG)

async_read_file.opt: async_read_file.ml
	ocamlfind ocamlopt -thread -package async,fmt $(OCAMLOPTFLAGS) -linkall -linkpkg async_read_file.ml -o async_read_file.opt

async_read_buffer.opt: async_read_buffer.ml
	ocamlfind ocamlopt -thread -package async,fmt $(OCAMLOPTFLAGS) -linkall -linkpkg async_read_buffer.ml -o async_read_buffer.opt

lwt_read_file.opt: lwt_read_file.ml
	ocamlfind ocamlopt -thread -package lwt,lwt.unix,fmt $(OCAMLOPTFLAGS) -linkall -linkpkg lwt_read_file.ml -o lwt_read_file.opt

lwt_read_buffer.opt: lwt_read_buffer.ml
	ocamlfind ocamlopt -thread -package lwt,lwt.unix,fmt $(OCAMLOPTFLAGS) -linkall -linkpkg lwt_read_buffer.ml -o lwt_read_buffer.opt

direct_read_file.opt: direct_read_file.ml
	ocamlfind ocamlopt -thread -package unix,fmt $(OCAMLOPTFLAGS) -linkall -linkpkg direct_read_file.ml -o direct_read_file.opt

direct_read_buffer.opt: direct_read_buffer.ml
	ocamlfind ocamlopt -thread -package unix,fmt $(OCAMLOPTFLAGS) -linkall -linkpkg direct_read_buffer.ml -o direct_read_buffer.opt

clean::
	rm -f *.cm* *.o *.opt *.byte
