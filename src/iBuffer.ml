open Event

  type t = {
    fd : Unix.file_descr ;
    buffer : bytes ;
    mutable ofs: int ;
    mutable lim : int ;
  }

  let create ~bufsiz fd =
    assert (bufsiz > 0);
    { fd = fd ; buffer = Bytes.create bufsiz ; ofs = bufsiz ; lim = bufsiz }

  let refill ibuf =
    let toread = Bytes.length ibuf.buffer in
    let nread = Unix.read ibuf.fd ibuf.buffer 0 toread in
      if nread = 0 then 0
      else begin
        ibuf.ofs <- 0 ;
        ibuf.lim <- nread ;
        nread
      end

  let read_char loop ibuf kont =
    if ibuf.ofs = Bytes.length ibuf.buffer then
      Loop.in_handler loop ibuf.fd 
        (fun fd ->
           Loop.in_cancel loop ibuf.fd ;
           let nread = refill ibuf in
           if nread = 0 then kont (-1)
           else begin
             let c = Char.code (Bytes.get ibuf.buffer 0) in
             ibuf.ofs <- 1 + ibuf.ofs;
             kont c
           end
        )
    else
      let c = Char.code (Bytes.get ibuf.buffer ibuf.ofs) in
      ibuf.ofs <- 1 + ibuf.ofs ;
      kont c
