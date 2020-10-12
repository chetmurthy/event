  type t = {
    fd : Unix.file_descr ;
    buffer : bytes ;
    mutable ofs: int ;
  }

  let create ~bufsiz fd =
    assert (bufsiz > 0);
    { fd = fd ; buffer = Bytes.create bufsiz ; ofs = bufsiz }

  let refill ibuf =
    Unix.read ibuf.fd ibuf.buffer 0 (Bytes.length ibuf.buffer)

  let rec read_char loop ibuf kont =
    if ibuf.ofs < Bytes.length ibuf.buffer then
      let c = Char.code (Bytes.get ibuf.buffer ibuf.ofs) in
      ibuf.ofs <- 1 + ibuf.ofs ;
      kont c
    else
      loop#in_handler ibuf.fd 
        (fun fd ->
           loop#in_cancel ibuf.fd ;
           let nread = refill ibuf in
           if nread = 0 then kont (-1)
           else begin
             ibuf.ofs <- 0 ;
             read_char loop ibuf kont
           end
        )
