
all: async_read_file.opt async_read_buffer.opt \
	lwt_read_file.opt lwt_read_buffer.opt \
	direct_read_file.opt direct_read_buffer.opt

F=/dev/zero
N=1048576
NLONG=10485760
test: all
	./async_read_file.opt $F 4096 $(NLONG)
	./async_read_file.opt $F 16 $N
	./async_read_buffer.opt $F $(NLONG)
	./lwt_read_file.opt $F 4096 $(NLONG)
	./lwt_read_file.opt $F 16 $N
	./lwt_read_buffer.opt $F $(NLONG)
	../tests/test_read_file.opt $F 4096 $(NLONG)
	../tests/test_read_file.opt $F 16 $N
	./direct_read_file.opt $F $N
	./direct_read_buffer.opt $F $(NLONG)

async_read_file.opt: async_read_file.ml
	ocamlfind ocamlopt -thread -package async -linkall -linkpkg async_read_file.ml -o async_read_file.opt

async_read_buffer.opt: async_read_buffer.ml
	ocamlfind ocamlopt -thread -package async -linkall -linkpkg async_read_buffer.ml -o async_read_buffer.opt

lwt_read_file.opt: lwt_read_file.ml
	ocamlfind ocamlopt -thread -package lwt,lwt.unix -linkall -linkpkg lwt_read_file.ml -o lwt_read_file.opt

lwt_read_buffer.opt: lwt_read_buffer.ml
	ocamlfind ocamlopt -thread -package lwt,lwt.unix -linkall -linkpkg lwt_read_buffer.ml -o lwt_read_buffer.opt

direct_read_file.opt: direct_read_file.ml
	ocamlfind ocamlopt -thread -package unix -linkall -linkpkg direct_read_file.ml -o direct_read_file.opt

direct_read_buffer.opt: direct_read_buffer.ml
	ocamlfind ocamlopt -thread -package unix -linkall -linkpkg direct_read_buffer.ml -o direct_read_buffer.opt

clean::
	rm -f *.cm* *.o *.opt *.byte
