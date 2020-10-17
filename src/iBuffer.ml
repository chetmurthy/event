open Event

  type t = {
    fd : Unix.file_descr ;
    buffer : bytes ;
    mutable ofs: int ;
  }

  let create ~bufsiz fd =
    assert (bufsiz > 0);
    { fd = fd ; buffer = Bytes.create bufsiz ; ofs = bufsiz }

  let refill ibuf =
    let rec rerec ofs toread =
      assert (ofs >= 0) ;
      assert (toread >= 0) ;
      assert (ofs < Bytes.length ibuf.buffer) ;
      assert (toread <= Bytes.length ibuf.buffer) ;
      let nread = Unix.read ibuf.fd ibuf.buffer ofs toread in
      if nread = 0 then 0
      else if nread = toread then ofs+nread
      else rerec (ofs+nread) (toread-nread) in
    let toread = Bytes.length ibuf.buffer in
    rerec 0 toread

  let read_char loop ibuf kont =
    if ibuf.ofs = Bytes.length ibuf.buffer then
      Loop.in_handler loop ibuf.fd 
        (fun fd ->
           Loop.in_cancel loop ibuf.fd ;
           let nread = refill ibuf in
           if nread = 0 then kont (-1)
           else begin
             let c = Char.code (Bytes.get ibuf.buffer 0) in
             ibuf.ofs <- 1 ;
             kont c
           end
        )
    else
      let c = Char.code (Bytes.get ibuf.buffer ibuf.ofs) in
      ibuf.ofs <- 1 + ibuf.ofs ;
      kont c
