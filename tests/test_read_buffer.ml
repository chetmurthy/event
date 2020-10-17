
open Event

let upd a b = a lor b

module Kont = struct
  let return v k = k v
  let bind _N _M = fun kont ->
    _N (fun v -> _M v kont)
end

let rec readn loop ibuf acc n kont =
  if n = 0 then Kont.return n kont
  else
    Kont.bind (fun kont -> JustBuffer.read_char loop ibuf kont)
      (fun c -> readn loop ibuf (upd c acc) (n-1)) kont

let report nread dur =
  let persec = Float.to_int ((Float.of_int nread) /. dur) in
  Fmt.(pf stdout "%a/sec: %d read in %f secs\n\n%!" bi_byte_size persec nread dur)

let main() =
  let bufsiz = int_of_string Sys.argv.(1) in
  let toread = int_of_string Sys.argv.(2) in
  let loop = Loop.create () in
  let ibuf = JustBuffer.create ~bufsiz in
  let stime = Unix.gettimeofday() in
  readn loop ibuf 0 toread (fun unread ->
      let etime = Unix.gettimeofday() in
      report  (toread-unread) (etime -. stime))

let _ = 
if not !Sys.interactive then
  main()
else ()
