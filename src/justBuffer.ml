open Event

  type t = {
    buffer : bytes ;
    mutable ofs: int ;
  }

  let create ~bufsiz =
    assert (bufsiz > 0);
    { buffer = Bytes.create bufsiz ; ofs = bufsiz }

  let rec read_char loop ibuf kont =
    if ibuf.ofs = Bytes.length ibuf.buffer then begin
      ibuf.ofs <- 0 ;
    end ;
    let c = Char.code (Bytes.unsafe_get ibuf.buffer ibuf.ofs) in
    ibuf.ofs <- 1 + ibuf.ofs ;
    kont c
