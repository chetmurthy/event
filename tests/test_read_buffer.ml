
open Event

let buffer = Bytes.create 1024 ;;

module Kont = struct
  let return v k = k v
  let bind _N _M = fun kont ->
    _N (fun v -> _M v kont)
end

let read1 n kont =
  Kont.return (Bytes.get buffer (n mod 1024)) kont

let rec readn n kont =
  if n = 0 then Kont.return n kont
  else
    Kont.bind (Kont.return (Bytes.get buffer (n mod 1024)))
      (fun _ -> readn (n-1)) kont

let report nread dur =
  let persec = Float.to_int ((Float.of_int nread) /. dur) in
  Fmt.(pf stdout "%a/sec: %d read in %f secs\n%!" bi_byte_size persec nread dur)

let main() =
  let fname = Sys.argv.(1) in
  let bufsiz = int_of_string Sys.argv.(2) in
  let toread = int_of_string Sys.argv.(3) in
  let loop = Loop.create () in
  let fd = Unix.openfile fname [Unix.O_RDONLY] 0o755 in
  let ibuf = IBuffer.create ~bufsiz fd in
  Unix.set_nonblock fd ;
  let stime = Unix.gettimeofday() in
  readn toread (fun unread ->
      let etime = Unix.gettimeofday() in
      report  (toread-unread) (etime -. stime) ;
      Loop.exit loop 0) ;
  Loop.loop loop

let _ = 
if not !Sys.interactive then
  main()
else ()
